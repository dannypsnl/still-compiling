#lang racket/base

(require rackunit
         rackunit/text-ui
         ffi/unsafe
         "dynasm.rkt"
         "unicorn.rkt")

(define CODE-ADDRESS #x10000)
(define RETURN-ADDRESS #x20000)
(define STACK-ADDRESS #x30000)

(define (get-code-bytes buf)
  (define code-ptr (dynasm-finalize buf))
  (define size (dynasm-pos buf))
  (define code-bytes (make-bytes size))
  (memcpy code-bytes code-ptr size)
  code-bytes)

(define (setup-uc code-bytes)
  (define uc (uc-create-arm64))
  (uc-map-memory uc CODE-ADDRESS #x1000)
  (uc-map-memory uc RETURN-ADDRESS #x1000)
  (uc-map-memory uc STACK-ADDRESS #x10000)
  (uc-write-code uc CODE-ADDRESS code-bytes)
  (uc-reg-write-u64 uc UC_ARM64_REG_X30 RETURN-ADDRESS)
  ;; Set up stack pointer in middle of stack region to allow growth both ways
  (uc-reg-write-u64 uc UC_ARM64_REG_SP (+ STACK-ADDRESS #x8000))
  uc)

(define (run-uc-void thunk)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (define code-bytes (get-code-bytes buf))
    (define uc (setup-uc code-bytes))
    (uc-emulate uc CODE-ADDRESS RETURN-ADDRESS)
    (define result (uc-reg-read-u64 uc UC_ARM64_REG_X0))
    (uc-close uc)
    (dynasm-free buf)
    result))

(define (run-uc-1arg thunk arg0)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (define code-bytes (get-code-bytes buf))
    (define uc (setup-uc code-bytes))
    (uc-reg-write-u64 uc UC_ARM64_REG_X0 arg0)
    (uc-emulate uc CODE-ADDRESS RETURN-ADDRESS)
    (define result (uc-reg-read-u64 uc UC_ARM64_REG_X0))
    (uc-close uc)
    (dynasm-free buf)
    result))

(define (run-uc-2arg thunk arg0 arg1)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (define code-bytes (get-code-bytes buf))

    (define uc (setup-uc code-bytes))
    (uc-reg-write-u64 uc UC_ARM64_REG_X0 arg0)
    (uc-reg-write-u64 uc UC_ARM64_REG_X1 arg1)

    (uc-emulate uc CODE-ADDRESS RETURN-ADDRESS)

    (define result (uc-reg-read-u64 uc UC_ARM64_REG_X0))

    (uc-close uc)
    (dynasm-free buf)
    result))

;; ============================================
;; x64 test helpers
;; ============================================

(define (run-uc-x64-with-regs code-bytes init-regs)
  (define CODE-ADDRESS #x1000000)
  (define STOP-ADDRESS (+ CODE-ADDRESS #x100000))
  (define STACK-TOP (+ CODE-ADDRESS #x200000))
  (define uc (uc-create-x64))
  ;; Map 2MB memory for emulation
  (uc-map-memory uc CODE-ADDRESS (* 2 1024 1024))
  (uc-write-code uc CODE-ADDRESS code-bytes)
  ;; Push return address onto stack so ret jumps to STOP-ADDRESS
  (define ret-addr-bytes (make-bytes 8))
  (integer->integer-bytes STOP-ADDRESS 8 #f #f ret-addr-bytes)
  (define rsp (- STACK-TOP 8))
  (uc-write-code uc rsp ret-addr-bytes)
  (uc-reg-write-u64 uc UC_X86_REG_RSP rsp)
  ;; Initialize registers
  (for ([reg-pair init-regs])
    (uc-reg-write-u64 uc (car reg-pair) (cdr reg-pair)))
  (uc-emu-start uc CODE-ADDRESS STOP-ADDRESS 0 0)
  (define result (uc-reg-read-u64 uc UC_X86_REG_RAX))
  (uc-close uc)
  result)

;; ============================================
;; RISC-V test helpers
;; ============================================

(define (run-uc-rv64-with-regs code-bytes init-regs)
  (define CODE-ADDRESS #x10000)
  (define STOP-ADDRESS #x20000)
  (define STACK-ADDRESS #x30000)
  (define uc (uc-create-riscv64))
  (uc-map-memory uc CODE-ADDRESS #x1000)
  (uc-map-memory uc STOP-ADDRESS #x1000)
  (uc-map-memory uc STACK-ADDRESS #x10000)
  (uc-write-code uc CODE-ADDRESS code-bytes)
  ;; Set RA to STOP-ADDRESS so ret works
  (uc-reg-write-u64 uc UC_RISCV_REG_RA STOP-ADDRESS)
  ;; Set SP to middle of stack region
  (uc-reg-write-u64 uc UC_RISCV_REG_SP (+ STACK-ADDRESS #x8000))
  ;; Initialize registers
  (for ([reg-pair init-regs])
    (uc-reg-write-u64 uc (car reg-pair) (cdr reg-pair)))
  (uc-emu-start uc CODE-ADDRESS STOP-ADDRESS 0 0)
  ;; Read result from A0 (X10)
  (define result (uc-reg-read-u64 uc UC_RISCV_REG_A0))
  (uc-close uc)
  result)

(define (run-rv64 thunk)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (define code-bytes (get-code-bytes buf))
    (define result (run-uc-rv64-with-regs code-bytes '()))
    (dynasm-free buf)
    result))

(define (run-rv64-with thunk init-regs)
  (let ([buf (dynasm-create 4096)])
    (thunk buf)
    (define code-bytes (get-code-bytes buf))
    (define result (run-uc-rv64-with-regs code-bytes init-regs))
    (dynasm-free buf)
    result))

(define tests
  (test-suite
   "Tests"

   (test-case "aarch64 - movz basic"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - movz zero value"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))))
      0))

   (test-case "aarch64 - movz max 16-bit"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFFFF 0))
         (emit! buf (aarch64-ret))))
      #xFFFF))

   (test-case "aarch64 - movz shift 16"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 1))
         (emit! buf (aarch64-ret))))
      #x10000))

   (test-case "aarch64 - movz shift 32"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 2))
         (emit! buf (aarch64-ret))))
      #x100000000))

   (test-case "aarch64 - movz shift 48"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 3))
         (emit! buf (aarch64-ret))))
      #x1000000000000))

   (test-case "aarch64 - movk build 32-bit"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #x5678 0))
         (emit! buf (aarch64-movk X0 #x1234 1))
         (emit! buf (aarch64-ret))))
      #x12345678))

   (test-case "aarch64 - movk build 64-bit"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xBABE 0))
         (emit! buf (aarch64-movk X0 #xCAFE 1))
         (emit! buf (aarch64-movk X0 #xBEEF 2))
         (emit! buf (aarch64-movk X0 #xDEAD 3))
         (emit! buf (aarch64-ret))))
      #xDEADBEEFCAFEBABE))

   (test-case "aarch64 - add_imm"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 10 0))
         (emit! buf (aarch64-add-imm X0 X0 32))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - sub_imm"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 100 0))
         (emit! buf (aarch64-sub-imm X0 X0 58))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - add_reg"
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       10 32)
      42))

   (test-case "aarch64 - sub_reg"
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-sub-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       100 58)
      42))

   (test-case "aarch64 - mul"
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       6 7)
      42))

   (test-case "aarch64 - sdiv"
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       42 6)
      7))

   (test-case "aarch64 - sum 1 to 10"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-movz X1 10 0))
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-sub-imm X1 X1 1))
         (emit! buf (aarch64-cmp-imm X1 0))
         (emit! buf (aarch64-b-cond COND-GT -3))
         (emit! buf (aarch64-ret))))
      55))

   (test-case "aarch64 - factorial 5"
     (check-equal?
      (run-uc-1arg
       (lambda (buf)
         (emit! buf (aarch64-movz X1 1 0))
         (emit! buf (aarch64-cmp-imm X0 1))
         (emit! buf (aarch64-b-cond COND-LE 4))
         (emit! buf (aarch64-mul X1 X1 X0))
         (emit! buf (aarch64-sub-imm X0 X0 1))
         (emit! buf (aarch64-b -4))
         (emit! buf (aarch64-add-imm X0 X1 0))
         (emit! buf (aarch64-ret)))
       5)
      120))

   (test-case "aarch64 - msub"
     ;; msub X0, X1, X2, X3 = X3 - (X1 * X2)
     ;; X1=3, X2=4, X3=20 -> 20 - 12 = 8
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X1 3 0))
         (emit! buf (aarch64-movz X2 4 0))
         (emit! buf (aarch64-movz X3 20 0))
         (emit! buf (aarch64-msub X0 X1 X2 X3))
         (emit! buf (aarch64-ret))))
      8))

   (test-case "aarch64 - lsr_imm"
     ;; 64 >> 2 = 16
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 64 0))
         (emit! buf (aarch64-lsr-imm X0 X0 2))
         (emit! buf (aarch64-ret))))
      16))

   (test-case "aarch64 - lsr_reg"
     ;; 64 >> 3 = 8
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-lsr-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       64 3)
      8))

   (test-case "aarch64 - lsl_imm"
     ;; 5 << 3 = 40
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 5 0))
         (emit! buf (aarch64-lsl-imm X0 X0 3))
         (emit! buf (aarch64-ret))))
      40))

   (test-case "aarch64 - lsl_reg"
     ;; 7 << 2 = 28
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-lsl-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       7 2)
      28))

   (test-case "aarch64 - and_imm"
     ;; 0xFF & 0x1 = 0x1 (only imm=1 is currently supported)
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFF 0))
         (emit! buf (aarch64-and-imm X0 X0 1))
         (emit! buf (aarch64-ret))))
      1))

   (test-case "aarch64 - clz"
     ;; 0xFF has 56 leading zeros (64-bit register)
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFF 0))
         (emit! buf (aarch64-clz X0 X0))
         (emit! buf (aarch64-ret))))
      56))

   (test-case "aarch64 - cmp_reg"
     ;; Compare X0 and X1, branch if not equal
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-cmp-reg X0 X1))
         (emit! buf (aarch64-b-cond COND-NE 2))
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 99 0))
         (emit! buf (aarch64-ret)))
       10 10)
      42))

   (test-case "aarch64 - csel"
     ;; if X2 > 0 then X0 = X0 else X0 = X1
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-movz X1 99 0))
         (emit! buf (aarch64-movz X2 5 0))
         (emit! buf (aarch64-cmp-imm X2 0))
         (emit! buf (aarch64-csel X0 X0 X1 COND-GT))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - csneg"
     ;; if X2 <= 0 then X0 = -X1 else X0 = X0
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-movz X1 10 0))
         (emit! buf (aarch64-movz X2 0 0))
         (emit! buf (aarch64-cmp-imm X2 0))
         (emit! buf (aarch64-csneg X0 X0 X1 COND-GT))
         (emit! buf (aarch64-ret))))
      ;; Since X2 == 0 (not > 0), return -10
      (- (expt 2 64) 10)))

   (test-case "aarch64 - nop"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - tst_imm"
     ;; TST sets flags without writing to a register
     ;; Test if bit 0 is set: 0xFE & 1 = 0, so Z flag is set, NE condition is false
     ;; If bit not set (Z=1), return 42, else return 99
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFE 0))  ; X0 = 0xFE (even, bit 0 clear)
         (emit! buf (aarch64-tst-imm X0 1))    ; Test bit 0, sets Z flag
         (emit! buf (aarch64-b-cond COND-NE 2)) ; If NE (bit was set), skip
         (emit! buf (aarch64-movz X0 42 0))    ; Bit not set, return 42
         (emit! buf (aarch64-ret))
         (emit! buf (aarch64-movz X0 99 0))    ; Bit was set, return 99
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - rbit"
     ;; Reverse bits of 0x1 (0x8000000000000000)
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-rbit X0 X0))
         (emit! buf (aarch64-ret))))
      #x8000000000000000))

   (test-case "aarch64 - ldr/str_imm"
     ;; Store 42 to [SP+16], then load it back (offset is in units of 8 bytes)
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-str-imm X0 SP 2))  ;; [SP + 2*8] = [SP + 16]
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ldr-imm X0 SP 2))  ;; [SP + 2*8] = [SP + 16]
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - str/ldr pair"
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X1 10 0))
         (emit! buf (aarch64-movz X2 20 0))
         (emit! buf (aarch64-str-imm X1 SP 0))  ;; Store X1 at [SP]
         (emit! buf (aarch64-str-imm X2 SP 1))  ;; Store X2 at [SP+8]
         (emit! buf (aarch64-movz X1 0 0))
         (emit! buf (aarch64-movz X2 0 0))
         (emit! buf (aarch64-ldr-imm X1 SP 0))  ;; Load X1 from [SP]
         (emit! buf (aarch64-ldr-imm X2 SP 1))  ;; Load X2 from [SP+8]
         (emit! buf (aarch64-add-reg X0 X1 X2))
         (emit! buf (aarch64-ret))))
      30))

   (test-case "aarch64 - fmov"
     ;; Simple test: move data from GP to SIMD and back
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-fmov-gp-to-vec V0 X0))  ; Move X0 -> V0
         (emit! buf (aarch64-movz X0 0 0))           ; Clear X0
         (emit! buf (aarch64-fmov-vec-to-gp X0 V0))  ; Move V0 -> X0
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - dup/umov"
     ;; DUP a GP register to all SIMD lanes, then extract one lane
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X1 42 0))
         (emit! buf (aarch64-dup-general V0 X1 SIMD-4S))  ; Duplicate X1 to all lanes of V0
         (emit! buf (aarch64-umov X0 V0 SIMD-4S 0))       ; Extract lane 0 to X0
         (emit! buf (aarch64-ret))))
      42))

   (test-case "aarch64 - add_simd"
     ;; Add two vectors and extract result
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X1 10 0))
         (emit! buf (aarch64-movz X2 32 0))
         (emit! buf (aarch64-dup-general V0 X1 SIMD-4S))  ; V0 = [10,10,10,10]
         (emit! buf (aarch64-dup-general V1 X2 SIMD-4S))  ; V1 = [32,32,32,32]
         (emit! buf (aarch64-add-simd V2 V0 V1 SIMD-4S))  ; V2 = V0 + V1
         (emit! buf (aarch64-umov X0 V2 SIMD-4S 0))       ; Extract lane 0
         (emit! buf (aarch64-ret))))
      42))

   (test-case "x64 - register read/write test"
     ;; Test that we can set and read registers correctly (no code execution)
     (define uc (uc-create-x64))
     (uc-map-memory uc CODE-ADDRESS #x1000)
     (uc-reg-write-u64 uc UC_X86_REG_RAX 42)
     (define result (uc-reg-read-u64 uc UC_X86_REG_RAX))
     (uc-close uc)
     (check-equal? result 42))

   (test-case "x64 - simple add using dynasm"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-add-reg RAX RBX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 10)
                                   (cons UC_X86_REG_RBX 22))))
     (dynasm-free buf)
     (check-equal? result 32))

   (test-case "x64 - mov_imm32"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-imm32 RAX 42))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - mov_imm64"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-imm64 RAX #xDEADBEEFCAFEBABE))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result #xDEADBEEFCAFEBABE))

   (test-case "x64 - mov_reg"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-reg RAX RBX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RBX 42))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - sub_reg"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-sub-reg RAX RBX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 100)
                                   (cons UC_X86_REG_RBX 58))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - add_imm32"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-add-imm32 RAX 32))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 10))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - add_imm8"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-add-imm8 RAX 7))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 35))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - sub_imm32"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-sub-imm32 RAX 58))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 100))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - sub_imm8"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-sub-imm8 RAX 8))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 50))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - imul_reg"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-imul-reg RAX RBX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 6)
                                   (cons UC_X86_REG_RBX 7))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - imul_imm32"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-imul-imm32 RAX RBX 7))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RBX 6))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - idiv_reg"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-cqo))
     (emit-x64! buf (x64-idiv-reg RBX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 84)
                                   (cons UC_X86_REG_RDX 0)
                                   (cons UC_X86_REG_RBX 2))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - inc"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-inc RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 41))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - dec"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-dec RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 43))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - neg"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-neg RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     ;; neg of -42 (two's complement) = 42
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX (- (expt 2 64) 42)))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - and_reg"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-and-reg RAX RBX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX #xFF)
                                   (cons UC_X86_REG_RBX #x2A))))
     (dynasm-free buf)
     (check-equal? result #x2A))

   (test-case "x64 - and_imm32"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-and-imm32 RAX #xFF))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX #x12A))))
     (dynasm-free buf)
     (check-equal? result #x2A))

   (test-case "x64 - or_reg"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-or-reg RAX RBX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX #x20)
                                   (cons UC_X86_REG_RBX #x0A))))
     (dynasm-free buf)
     (check-equal? result #x2A))

   (test-case "x64 - xor_reg"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-imm32 RAX 999))
     (emit-x64! buf (x64-xor-reg RAX RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result 0))

   (test-case "x64 - shl_imm"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-shl-imm RAX 1))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 21))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - shr_imm"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-shr-imm RAX 1))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 84))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - sar_imm"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-sar-imm RAX 2))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 168))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - shl_cl"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-shl-cl RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 21)
                                   (cons UC_X86_REG_RCX 1))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - shr_cl"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-shr-cl RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 84)
                                   (cons UC_X86_REG_RCX 1))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - sar_cl"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-sar-cl RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 168)
                                   (cons UC_X86_REG_RCX 2))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - cmp_reg + jcc_rel8 (equal)"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-cmp-reg RAX RBX))
     (emit-x64! buf (x64-jcc-rel8 CC-E 8))       ; if equal, skip next mov+ret (7+1 bytes)
     (emit-x64! buf (x64-mov-imm32 RAX 99))       ; not equal path
     (emit-x64! buf (x64-ret))
     (emit-x64! buf (x64-mov-imm32 RAX 42))       ; equal path
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 10)
                                   (cons UC_X86_REG_RBX 10))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - cmp_reg + jcc_rel8 (not equal)"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-cmp-reg RAX RBX))
     (emit-x64! buf (x64-jcc-rel8 CC-NE 8))       ; if not equal, skip next mov+ret (7+1 bytes)
     (emit-x64! buf (x64-mov-imm32 RAX 99))        ; equal path
     (emit-x64! buf (x64-ret))
     (emit-x64! buf (x64-mov-imm32 RAX 42))        ; not equal path
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 10)
                                   (cons UC_X86_REG_RBX 20))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - cmp_imm32 + jcc_rel8 (less)"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-cmp-imm32 RAX 100))
     (emit-x64! buf (x64-jcc-rel8 CC-L 8))         ; if RAX < 100, jump over mov+ret (7+1 bytes)
     (emit-x64! buf (x64-mov-imm32 RAX 99))         ; not less path
     (emit-x64! buf (x64-ret))
     (emit-x64! buf (x64-mov-imm32 RAX 42))         ; less path
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 50))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - cmp_imm8"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-cmp-imm8 RAX 50))
     (emit-x64! buf (x64-jcc-rel8 CC-GE 8))        ; if RAX >= 50, jump over mov+ret (7+1 bytes)
     (emit-x64! buf (x64-mov-imm32 RAX 99))
     (emit-x64! buf (x64-ret))
     (emit-x64! buf (x64-mov-imm32 RAX 42))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 100))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - test_reg + jcc (zero check)"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-test-reg RAX RAX))
     (emit-x64! buf (x64-jcc-rel8 CC-Z 8))          ; if zero, jump over mov+ret (7+1 bytes)
     (emit-x64! buf (x64-mov-imm32 RAX 99))          ; non-zero path
     (emit-x64! buf (x64-ret))
     (emit-x64! buf (x64-mov-imm32 RAX 42))          ; zero path
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 0))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - jmp_rel8"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-jmp-rel8 7))               ; skip next mov (7 bytes)
     (emit-x64! buf (x64-mov-imm32 RAX 99))
     (emit-x64! buf (x64-mov-imm32 RAX 42))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - jmp_rel32"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-jmp-rel32 7))              ; skip next mov (7 bytes)
     (emit-x64! buf (x64-mov-imm32 RAX 99))
     (emit-x64! buf (x64-mov-imm32 RAX 42))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - jcc_rel32 (CC-G)"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-cmp-reg RAX RBX))
     (emit-x64! buf (x64-jcc-rel32 CC-G 8))         ; if RAX > RBX, jump over next mov+ret
     (emit-x64! buf (x64-mov-imm32 RAX 99))
     (emit-x64! buf (x64-ret))
     (emit-x64! buf (x64-mov-imm32 RAX 42))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 50)
                                   (cons UC_X86_REG_RBX 20))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - push and pop"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-push RAX))
     (emit-x64! buf (x64-mov-imm32 RAX 0))
     (emit-x64! buf (x64-pop RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 42))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - nop"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-imm32 RAX 42))
     (emit-x64! buf (x64-nop))
     (emit-x64! buf (x64-nop))
     (emit-x64! buf (x64-nop))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - mov_mr and mov_rm (memory store/load)"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-sub-imm8 RSP 8))
     (emit-x64! buf (x64-mov-mr RSP RAX))
     (emit-x64! buf (x64-mov-imm32 RAX 0))
     (emit-x64! buf (x64-mov-rm RAX RSP))
     (emit-x64! buf (x64-add-imm8 RSP 8))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 42))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - mov_mr_disp32 and mov_rm_disp32"
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-sub-imm8 RSP 16))
     (emit-x64! buf (x64-mov-mr-disp32 RSP 8 RAX))   ; store at [RSP+8]
     (emit-x64! buf (x64-mov-imm32 RAX 0))
     (emit-x64! buf (x64-mov-rm-disp32 RAX RSP 8))    ; load from [RSP+8]
     (emit-x64! buf (x64-add-imm8 RSP 16))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 42))))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - combined arithmetic (add + sub + imul)"
     ;; (5 + 3) * 7 - 14 = 42
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-imm32 RAX 5))
     (emit-x64! buf (x64-add-imm8 RAX 3))
     (emit-x64! buf (x64-imul-imm32 RAX RAX 7))
     (emit-x64! buf (x64-sub-imm8 RAX 14))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - R8-R15 registers"
     ;; Test that extended registers work: move through R8
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-reg R8 RAX))
     (emit-x64! buf (x64-mov-reg RAX R8))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result
       (run-uc-x64-with-regs code-bytes
                             (list (cons UC_X86_REG_RAX 42))))
     (dynasm-free buf)
     (check-equal? result 42))

   ;; ============================================
   ;; RISC-V 64-bit instruction tests
   ;; ============================================

   ;; --- R-type arithmetic ---

   (test-case "rv64 - add"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-add RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)
             (cons UC_RISCV_REG_X12 32)))
      42))

   (test-case "rv64 - sub"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sub RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 100)
             (cons UC_RISCV_REG_X12 58)))
      42))

   (test-case "rv64 - sll"
     ;; 21 << 1 = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sll RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 21)
             (cons UC_RISCV_REG_X12 1)))
      42))

   (test-case "rv64 - slt (less)"
     ;; 5 < 10 → 1
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-slt RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 5)
             (cons UC_RISCV_REG_X12 10)))
      1))

   (test-case "rv64 - slt (not less)"
     ;; 10 < 5 → 0
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-slt RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)
             (cons UC_RISCV_REG_X12 5)))
      0))

   (test-case "rv64 - sltu"
     ;; unsigned: 5 < 10 → 1
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sltu RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 5)
             (cons UC_RISCV_REG_X12 10)))
      1))

   (test-case "rv64 - xor"
     ;; 0xFF ^ 0xD5 = 0x2A = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-xor RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 #xFF)
             (cons UC_RISCV_REG_X12 #xD5)))
      #x2A))

   (test-case "rv64 - srl"
     ;; 84 >> 1 = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-srl RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 84)
             (cons UC_RISCV_REG_X12 1)))
      42))

   (test-case "rv64 - sra"
     ;; 168 >> 2 = 42 (positive value, same as srl)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sra RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 168)
             (cons UC_RISCV_REG_X12 2)))
      42))

   (test-case "rv64 - or"
     ;; 0x20 | 0x0A = 0x2A = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-or RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 #x20)
             (cons UC_RISCV_REG_X12 #x0A)))
      #x2A))

   (test-case "rv64 - and"
     ;; 0xFF & 0x2A = 0x2A = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-and RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 #xFF)
             (cons UC_RISCV_REG_X12 #x2A)))
      #x2A))

   ;; --- W-type (32-bit word operations) ---

   (test-case "rv64 - addw"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-addw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)
             (cons UC_RISCV_REG_X12 32)))
      42))

   (test-case "rv64 - subw"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-subw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 100)
             (cons UC_RISCV_REG_X12 58)))
      42))

   (test-case "rv64 - sllw"
     ;; 21 << 1 = 42 (32-bit)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sllw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 21)
             (cons UC_RISCV_REG_X12 1)))
      42))

   (test-case "rv64 - srlw"
     ;; 84 >> 1 = 42 (32-bit logical)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-srlw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 84)
             (cons UC_RISCV_REG_X12 1)))
      42))

   (test-case "rv64 - sraw"
     ;; 168 >> 2 = 42 (32-bit arithmetic, positive)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sraw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 168)
             (cons UC_RISCV_REG_X12 2)))
      42))

   ;; --- I-type immediate arithmetic ---

   (test-case "rv64 - addi"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-addi RV-A0 RV-A1 32))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)))
      42))

   (test-case "rv64 - slti (less)"
     ;; 5 < 100 → 1
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-slti RV-A0 RV-A1 100))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 5)))
      1))

   (test-case "rv64 - sltiu"
     ;; unsigned: 5 < 100 → 1
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sltiu RV-A0 RV-A1 100))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 5)))
      1))

   (test-case "rv64 - xori"
     ;; 0xFF ^ 0xD5 = 0x2A
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-xori RV-A0 RV-A1 #xD5))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 #xFF)))
      #x2A))

   (test-case "rv64 - ori"
     ;; 0x20 | 0x0A = 0x2A
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-ori RV-A0 RV-A1 #x0A))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 #x20)))
      #x2A))

   (test-case "rv64 - andi"
     ;; 0xFF & 0x2A = 0x2A
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-andi RV-A0 RV-A1 #x2A))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 #xFF)))
      #x2A))

   (test-case "rv64 - slli"
     ;; 21 << 1 = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-slli RV-A0 RV-A1 1))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 21)))
      42))

   (test-case "rv64 - srli"
     ;; 84 >> 1 = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-srli RV-A0 RV-A1 1))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 84)))
      42))

   (test-case "rv64 - srai"
     ;; 168 >> 2 = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-srai RV-A0 RV-A1 2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 168)))
      42))

   ;; --- W immediate variants ---

   (test-case "rv64 - addiw"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-addiw RV-A0 RV-A1 32))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)))
      42))

   (test-case "rv64 - slliw"
     ;; 21 << 1 = 42 (32-bit)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-slliw RV-A0 RV-A1 1))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 21)))
      42))

   (test-case "rv64 - srliw"
     ;; 84 >> 1 = 42 (32-bit logical)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-srliw RV-A0 RV-A1 1))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 84)))
      42))

   (test-case "rv64 - sraiw"
     ;; 168 >> 2 = 42 (32-bit arithmetic)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sraiw RV-A0 RV-A1 2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 168)))
      42))

   ;; --- Load/Store ---

   (test-case "rv64 - sd/ld (doubleword)"
     ;; Store 42 to [SP+0], load it back
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sd RV-A1 RV-SP 0))
         (emit! buf (riscv64-ld RV-A0 RV-SP 0))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   (test-case "rv64 - sw/lw (word, sign-extended)"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sw RV-A1 RV-SP 0))
         (emit! buf (riscv64-lw RV-A0 RV-SP 0))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   (test-case "rv64 - sw/lwu (word, zero-extended)"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sw RV-A1 RV-SP 0))
         (emit! buf (riscv64-lwu RV-A0 RV-SP 0))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   (test-case "rv64 - sh/lh (halfword, sign-extended)"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sh RV-A1 RV-SP 0))
         (emit! buf (riscv64-lh RV-A0 RV-SP 0))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   (test-case "rv64 - sh/lhu (halfword, zero-extended)"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sh RV-A1 RV-SP 0))
         (emit! buf (riscv64-lhu RV-A0 RV-SP 0))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   (test-case "rv64 - sb/lb (byte, sign-extended)"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sb RV-A1 RV-SP 0))
         (emit! buf (riscv64-lb RV-A0 RV-SP 0))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   (test-case "rv64 - sb/lbu (byte, zero-extended)"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sb RV-A1 RV-SP 0))
         (emit! buf (riscv64-lbu RV-A0 RV-SP 0))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   (test-case "rv64 - sd/ld with offset"
     ;; Store at [SP+16], load from [SP+16]
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-sd RV-A1 RV-SP 16))
         (emit! buf (riscv64-ld RV-A0 RV-SP 16))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   ;; --- Upper immediate ---

   (test-case "rv64 - lui"
     ;; lui a0, #x1000 → a0 = 4096 (upper bits already in position)
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-lui RV-A0 #x1000))
         (emit! buf (riscv64-ret))))
      4096))

   (test-case "rv64 - lui + addi"
     ;; lui a0, #x1000; addi a0, a0, 42 → a0 = 4096 + 42 = 4138
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-lui RV-A0 #x1000))
         (emit! buf (riscv64-addi RV-A0 RV-A0 42))
         (emit! buf (riscv64-ret))))
      4138))

   (test-case "rv64 - auipc"
     ;; auipc a0, 0 → a0 = PC = CODE-ADDRESS = 0x10000
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-auipc RV-A0 0))
         (emit! buf (riscv64-ret))))
      #x10000))

   ;; --- Branch instructions ---

   (test-case "rv64 - beq (taken)"
     ;; beq a1, a2, 12 → skip li+ret, land on second li
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-beq RV-A1 RV-A2 12))    ; PC+0: if a1==a2, jump to PC+12
         (emit! buf (riscv64-li RV-A0 99))            ; PC+4: not-taken
         (emit! buf (riscv64-ret))                     ; PC+8
         (emit! buf (riscv64-li RV-A0 42))            ; PC+12: taken
         (emit! buf (riscv64-ret)))                    ; PC+16
       (list (cons UC_RISCV_REG_X11 10)
             (cons UC_RISCV_REG_X12 10)))
      42))

   (test-case "rv64 - beq (not taken)"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-beq RV-A1 RV-A2 12))
         (emit! buf (riscv64-li RV-A0 42))
         (emit! buf (riscv64-ret))
         (emit! buf (riscv64-li RV-A0 99))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)
             (cons UC_RISCV_REG_X12 20)))
      42))

   (test-case "rv64 - bne (taken)"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-bne RV-A1 RV-A2 12))
         (emit! buf (riscv64-li RV-A0 99))
         (emit! buf (riscv64-ret))
         (emit! buf (riscv64-li RV-A0 42))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)
             (cons UC_RISCV_REG_X12 20)))
      42))

   (test-case "rv64 - blt (taken)"
     ;; 5 < 10 → taken
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-blt RV-A1 RV-A2 12))
         (emit! buf (riscv64-li RV-A0 99))
         (emit! buf (riscv64-ret))
         (emit! buf (riscv64-li RV-A0 42))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 5)
             (cons UC_RISCV_REG_X12 10)))
      42))

   (test-case "rv64 - bge (taken)"
     ;; 10 >= 5 → taken
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-bge RV-A1 RV-A2 12))
         (emit! buf (riscv64-li RV-A0 99))
         (emit! buf (riscv64-ret))
         (emit! buf (riscv64-li RV-A0 42))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)
             (cons UC_RISCV_REG_X12 5)))
      42))

   (test-case "rv64 - bltu (taken)"
     ;; unsigned: 5 < 10 → taken
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-bltu RV-A1 RV-A2 12))
         (emit! buf (riscv64-li RV-A0 99))
         (emit! buf (riscv64-ret))
         (emit! buf (riscv64-li RV-A0 42))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 5)
             (cons UC_RISCV_REG_X12 10)))
      42))

   (test-case "rv64 - bgeu (taken)"
     ;; unsigned: 10 >= 5 → taken
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-bgeu RV-A1 RV-A2 12))
         (emit! buf (riscv64-li RV-A0 99))
         (emit! buf (riscv64-ret))
         (emit! buf (riscv64-li RV-A0 42))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 10)
             (cons UC_RISCV_REG_X12 5)))
      42))

   ;; --- Jump instructions ---

   (test-case "rv64 - jal"
     ;; jal x0, 8 → jump to PC+8 (skip 1 instruction)
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-jal RV-ZERO 8))          ; PC+0: jump to PC+8
         (emit! buf (riscv64-li RV-A0 99))            ; PC+4: skipped
         (emit! buf (riscv64-li RV-A0 42))            ; PC+8: target
         (emit! buf (riscv64-ret))))
      42))

   (test-case "rv64 - jal (saves return address)"
     ;; jal a1, 8 → a1 = PC+4, jump to PC+8
     ;; Check that a1 contains the return address (CODE-ADDRESS + 4)
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-jal RV-A1 8))            ; PC+0: a1 = PC+4, jump to PC+8
         (emit! buf (riscv64-li RV-A0 99))            ; PC+4: skipped
         (emit! buf (riscv64-addi RV-A0 RV-A1 0))    ; PC+8: a0 = a1 = CODE-ADDRESS + 4
         (emit! buf (riscv64-ret))))
      (+ #x10000 4)))

   (test-case "rv64 - jalr"
     ;; Use auipc to get PC, add offset, then jalr
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-auipc RV-T0 0))          ; PC+0: t0 = PC
         (emit! buf (riscv64-addi RV-T0 RV-T0 16))    ; PC+4: t0 = PC+16
         (emit! buf (riscv64-jalr RV-ZERO RV-T0 0))   ; PC+8: jump to t0 (PC+16)
         (emit! buf (riscv64-li RV-A0 99))            ; PC+12: skipped
         (emit! buf (riscv64-li RV-A0 42))            ; PC+16: target
         (emit! buf (riscv64-ret))))
      42))

   (test-case "rv64 - j (unconditional jump)"
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-j 8))                     ; PC+0: jump to PC+8
         (emit! buf (riscv64-li RV-A0 99))            ; PC+4: skipped
         (emit! buf (riscv64-li RV-A0 42))            ; PC+8: target
         (emit! buf (riscv64-ret))))
      42))

   (test-case "rv64 - jr"
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-auipc RV-T0 0))          ; PC+0: t0 = PC
         (emit! buf (riscv64-addi RV-T0 RV-T0 16))    ; PC+4: t0 = PC+16
         (emit! buf (riscv64-jr RV-T0))                ; PC+8: jump to t0
         (emit! buf (riscv64-li RV-A0 99))            ; PC+12: skipped
         (emit! buf (riscv64-li RV-A0 42))            ; PC+16: target
         (emit! buf (riscv64-ret))))
      42))

   ;; --- RV64M Multiply/Divide ---

   (test-case "rv64 - mul"
     ;; 6 * 7 = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-mul RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 6)
             (cons UC_RISCV_REG_X12 7)))
      42))

   (test-case "rv64 - mulh (upper 64 bits, small values)"
     ;; Small values: upper 64 bits = 0
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-mulh RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 6)
             (cons UC_RISCV_REG_X12 7)))
      0))

   (test-case "rv64 - mulh (upper 64 bits, large values)"
     ;; 2^32 * 2^32 = 2^64, upper 64 bits = 1
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-mulh RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 (expt 2 32))
             (cons UC_RISCV_REG_X12 (expt 2 32))))
      1))

   (test-case "rv64 - mulhu"
     ;; 2^32 * 2^32 = 2^64, upper 64 bits = 1 (unsigned)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-mulhu RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 (expt 2 32))
             (cons UC_RISCV_REG_X12 (expt 2 32))))
      1))

   (test-case "rv64 - mulhsu"
     ;; signed * unsigned with small positive values → 0
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-mulhsu RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 6)
             (cons UC_RISCV_REG_X12 7)))
      0))

   (test-case "rv64 - div"
     ;; 84 / 2 = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-div RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 84)
             (cons UC_RISCV_REG_X12 2)))
      42))

   (test-case "rv64 - divu"
     ;; unsigned: 84 / 2 = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-divu RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 84)
             (cons UC_RISCV_REG_X12 2)))
      42))

   (test-case "rv64 - rem"
     ;; 47 % 5 = 2
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-rem RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 47)
             (cons UC_RISCV_REG_X12 5)))
      2))

   (test-case "rv64 - remu"
     ;; unsigned: 47 % 5 = 2
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-remu RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 47)
             (cons UC_RISCV_REG_X12 5)))
      2))

   ;; --- RV64M Word variants ---

   (test-case "rv64 - mulw"
     ;; 6 * 7 = 42 (32-bit)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-mulw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 6)
             (cons UC_RISCV_REG_X12 7)))
      42))

   (test-case "rv64 - divw"
     ;; 84 / 2 = 42 (32-bit signed)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-divw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 84)
             (cons UC_RISCV_REG_X12 2)))
      42))

   (test-case "rv64 - divuw"
     ;; 84 / 2 = 42 (32-bit unsigned)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-divuw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 84)
             (cons UC_RISCV_REG_X12 2)))
      42))

   (test-case "rv64 - remw"
     ;; 47 % 5 = 2 (32-bit signed)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-remw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 47)
             (cons UC_RISCV_REG_X12 5)))
      2))

   (test-case "rv64 - remuw"
     ;; 47 % 5 = 2 (32-bit unsigned)
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-remuw RV-A0 RV-A1 RV-A2))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 47)
             (cons UC_RISCV_REG_X12 5)))
      2))

   ;; --- Pseudo-instructions ---

   (test-case "rv64 - nop"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-li RV-A0 42))
         (emit! buf (riscv64-nop))
         (emit! buf (riscv64-nop))
         (emit! buf (riscv64-ret)))
       '())
      42))

   (test-case "rv64 - mv"
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-mv RV-A0 RV-A1))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 42)))
      42))

   (test-case "rv64 - not"
     ;; not 0 = 0xFFFFFFFFFFFFFFFF
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-not RV-A0 RV-A1))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 0)))
      (sub1 (expt 2 64))))

   (test-case "rv64 - neg"
     ;; neg(-42) = 42
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-neg RV-A0 RV-A1))
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X11 (- (expt 2 64) 42))))
      42))

   (test-case "rv64 - li"
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-li RV-A0 42))
         (emit! buf (riscv64-ret))))
      42))

   (test-case "rv64 - li negative"
     ;; li a0, -1 → a0 = 0xFFFFFFFFFFFFFFFF
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-li RV-A0 -1))
         (emit! buf (riscv64-ret))))
      (sub1 (expt 2 64))))

   ;; --- Combined tests ---

   (test-case "rv64 - sum 1 to 10"
     ;; a0 = 0 (sum), a1 = 10 (counter)
     ;; loop: a0 += a1; a1 -= 1; if a1 > 0, goto loop
     (check-equal?
      (run-rv64
       (lambda (buf)
         (emit! buf (riscv64-li RV-A0 0))             ; sum = 0
         (emit! buf (riscv64-li RV-A1 10))            ; counter = 10
         ;; loop (PC+8):
         (emit! buf (riscv64-add RV-A0 RV-A0 RV-A1)) ; sum += counter
         (emit! buf (riscv64-addi RV-A1 RV-A1 -1))   ; counter -= 1
         (emit! buf (riscv64-blt RV-ZERO RV-A1 -8))   ; if 0 < counter, jump back 8 bytes
         (emit! buf (riscv64-ret))))
      55))

   (test-case "rv64 - factorial 5"
     ;; a0 = input (5), a1 = result (1)
     ;; loop: if a0 <= 1, done; result *= a0; a0 -= 1; goto loop
     (check-equal?
      (run-rv64-with
       (lambda (buf)
         (emit! buf (riscv64-li RV-A1 1))             ; result = 1
         (emit! buf (riscv64-li RV-T0 1))             ; const 1
         ;; loop (PC+8):
         (emit! buf (riscv64-bge RV-T0 RV-A0 16))    ; if 1 >= a0, jump to done (PC+8+16=PC+24)
         (emit! buf (riscv64-mul RV-A1 RV-A1 RV-A0))  ; result *= a0
         (emit! buf (riscv64-addi RV-A0 RV-A0 -1))   ; a0 -= 1
         (emit! buf (riscv64-j -12))                   ; jump back to loop (PC+20 -> PC+8 = -12)
         ;; done (PC+24):
         (emit! buf (riscv64-mv RV-A0 RV-A1))         ; return result
         (emit! buf (riscv64-ret)))
       (list (cons UC_RISCV_REG_X10 5)))
      120))
   ))

(module+ main
  (run-tests tests 'verbose))

(module+ test
  (run-tests tests))
