#lang racket/base

(require rackunit
         rackunit/text-ui
         ffi/unsafe
         "dynasm.rkt"
         "unicorn.rkt")

(displayln "Starting test...")

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

(define tests
  (test-suite
   "Tests"

   (test-case "movz - basic"
     (displayln "  movz basic...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "movz - zero value"
     (displayln "  movz zero...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 0 0))
         (emit! buf (aarch64-ret))))
      0))

   (test-case "movz - max 16-bit"
     (displayln "  movz max...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFFFF 0))
         (emit! buf (aarch64-ret))))
      #xFFFF))

   (test-case "movz - shift 16"
     (displayln "  movz shift 16...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 1))
         (emit! buf (aarch64-ret))))
      #x10000))

   (test-case "movz - shift 32"
     (displayln "  movz shift 32...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 2))
         (emit! buf (aarch64-ret))))
      #x100000000))

   (test-case "movz - shift 48"
     (displayln "  movz shift 48...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 3))
         (emit! buf (aarch64-ret))))
      #x1000000000000))

   (test-case "movk - build 32-bit"
     (displayln "  movk 32-bit...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #x5678 0))
         (emit! buf (aarch64-movk X0 #x1234 1))
         (emit! buf (aarch64-ret))))
      #x12345678))

   (test-case "movk - build 64-bit"
     (displayln "  movk 64-bit...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xBABE 0))
         (emit! buf (aarch64-movk X0 #xCAFE 1))
         (emit! buf (aarch64-movk X0 #xBEEF 2))
         (emit! buf (aarch64-movk X0 #xDEAD 3))
         (emit! buf (aarch64-ret))))
      #xDEADBEEFCAFEBABE))

   (test-case "add_imm - basic"
     (displayln "  add_imm...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 10 0))
         (emit! buf (aarch64-add-imm X0 X0 32))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "sub_imm - basic"
     (displayln "  sub_imm...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 100 0))
         (emit! buf (aarch64-sub-imm X0 X0 58))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "add_reg - basic"
     (displayln "  add_reg...")
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-add-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       10 32)
      42))

   (test-case "sub_reg - basic"
     (displayln "  sub_reg...")
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-sub-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       100 58)
      42))

   (test-case "mul - basic"
     (displayln "  mul...")
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-mul X0 X0 X1))
         (emit! buf (aarch64-ret)))
       6 7)
      42))

   (test-case "sdiv - basic"
     (displayln "  sdiv...")
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-sdiv X0 X0 X1))
         (emit! buf (aarch64-ret)))
       42 6)
      7))

   (test-case "sum 1 to 10"
     (displayln "  sum loop...")
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

   (test-case "factorial 5"
     (displayln "  factorial...")
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

   (test-case "msub - multiply-subtract"
     (displayln "  msub...")
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

   (test-case "lsr_imm - logical shift right immediate"
     (displayln "  lsr_imm...")
     ;; 64 >> 2 = 16
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 64 0))
         (emit! buf (aarch64-lsr-imm X0 X0 2))
         (emit! buf (aarch64-ret))))
      16))

   (test-case "lsr_reg - logical shift right register"
     (displayln "  lsr_reg...")
     ;; 64 >> 3 = 8
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-lsr-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       64 3)
      8))

   (test-case "lsl_imm - logical shift left immediate"
     (displayln "  lsl_imm...")
     ;; 5 << 3 = 40
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 5 0))
         (emit! buf (aarch64-lsl-imm X0 X0 3))
         (emit! buf (aarch64-ret))))
      40))

   (test-case "lsl_reg - logical shift left register"
     (displayln "  lsl_reg...")
     ;; 7 << 2 = 28
     (check-equal?
      (run-uc-2arg
       (lambda (buf)
         (emit! buf (aarch64-lsl-reg X0 X0 X1))
         (emit! buf (aarch64-ret)))
       7 2)
      28))

   (test-case "and_imm - bitwise AND immediate"
     (displayln "  and_imm...")
     ;; 0xFF & 0x1 = 0x1 (only imm=1 is currently supported)
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFF 0))
         (emit! buf (aarch64-and-imm X0 X0 1))
         (emit! buf (aarch64-ret))))
      1))

   (test-case "clz - count leading zeros"
     (displayln "  clz...")
     ;; 0xFF has 56 leading zeros (64-bit register)
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 #xFF 0))
         (emit! buf (aarch64-clz X0 X0))
         (emit! buf (aarch64-ret))))
      56))

   (test-case "cmp_reg - compare registers"
     (displayln "  cmp_reg...")
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

   (test-case "csel - conditional select"
     (displayln "  csel...")
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

   (test-case "csneg - conditional select negated"
     (displayln "  csneg...")
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

   (test-case "nop - no operation"
     (displayln "  nop...")
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 42 0))
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-nop))
         (emit! buf (aarch64-ret))))
      42))

   (test-case "tst_imm - test bits immediate"
     (displayln "  tst_imm...")
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

   (test-case "rbit - reverse bits"
     (displayln "  rbit...")
     ;; Reverse bits of 0x1 (0x8000000000000000)
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X0 1 0))
         (emit! buf (aarch64-rbit X0 X0))
         (emit! buf (aarch64-ret))))
      #x8000000000000000))

   (test-case "ldr/str_imm - load/store with offset"
     (displayln "  ldr/str_imm...")
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

   (test-case "store/load two values (pair functionality)"
     (displayln "  str/ldr pair...")
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

   (test-case "fmov - move between GP and SIMD registers"
     (displayln "  fmov...")
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

   (test-case "dup/umov - SIMD element operations"
     (displayln "  dup/umov...")
     ;; DUP a GP register to all SIMD lanes, then extract one lane
     (check-equal?
      (run-uc-void
       (lambda (buf)
         (emit! buf (aarch64-movz X1 42 0))
         (emit! buf (aarch64-dup-general V0 X1 SIMD-4S))  ; Duplicate X1 to all lanes of V0
         (emit! buf (aarch64-umov X0 V0 SIMD-4S 0))       ; Extract lane 0 to X0
         (emit! buf (aarch64-ret))))
      42))

   (test-case "add_simd - SIMD vector addition"
     (displayln "  add_simd...")
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
     (displayln "  x64 reg r/w...")
     ;; Test that we can set and read registers correctly (no code execution)
     (define uc (uc-create-x64))
     (uc-map-memory uc CODE-ADDRESS #x1000)
     (uc-reg-write-u64 uc UC_X86_REG_RAX 42)
     (define result (uc-reg-read-u64 uc UC_X86_REG_RAX))
     (uc-close uc)
     (check-equal? result 42))

   (test-case "x64 - simple add using dynasm"
     (displayln "  x64 dynasm add...")
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
     (displayln "  x64 mov_imm32...")
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-imm32 RAX 42))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result 42))

   (test-case "x64 - mov_imm64"
     (displayln "  x64 mov_imm64...")
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-imm64 RAX #xDEADBEEFCAFEBABE))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result #xDEADBEEFCAFEBABE))

   (test-case "x64 - mov_reg"
     (displayln "  x64 mov_reg...")
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
     (displayln "  x64 sub_reg...")
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
     (displayln "  x64 add_imm32...")
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
     (displayln "  x64 add_imm8...")
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
     (displayln "  x64 sub_imm32...")
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
     (displayln "  x64 sub_imm8...")
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
     (displayln "  x64 imul_reg...")
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
     (displayln "  x64 imul_imm32...")
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
     (displayln "  x64 idiv_reg...")
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
     (displayln "  x64 inc...")
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
     (displayln "  x64 dec...")
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
     (displayln "  x64 neg...")
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
     (displayln "  x64 and_reg...")
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
     (displayln "  x64 and_imm32...")
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
     (displayln "  x64 or_reg...")
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
     (displayln "  x64 xor_reg...")
     (define buf (dynasm-create 4096))
     (emit-x64! buf (x64-mov-imm32 RAX 999))
     (emit-x64! buf (x64-xor-reg RAX RAX))
     (emit-x64! buf (x64-ret))
     (define code-bytes (get-code-bytes buf))
     (define result (run-uc-x64-with-regs code-bytes '()))
     (dynasm-free buf)
     (check-equal? result 0))

   (test-case "x64 - shl_imm"
     (displayln "  x64 shl_imm...")
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
     (displayln "  x64 shr_imm...")
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
     (displayln "  x64 sar_imm...")
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
     (displayln "  x64 shl_cl...")
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
     (displayln "  x64 shr_cl...")
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
     (displayln "  x64 sar_cl...")
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
     (displayln "  x64 cmp_reg + jcc_rel8 equal...")
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
     (displayln "  x64 cmp_reg + jcc_rel8 not equal...")
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
     (displayln "  x64 cmp_imm32 + jcc_rel8 less...")
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
     (displayln "  x64 cmp_imm8...")
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
     (displayln "  x64 test_reg...")
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
     (displayln "  x64 jmp_rel8...")
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
     (displayln "  x64 jmp_rel32...")
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
     (displayln "  x64 jcc_rel32...")
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
     (displayln "  x64 push/pop...")
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
     (displayln "  x64 nop...")
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
     (displayln "  x64 mov_mr/mov_rm...")
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
     (displayln "  x64 mov_mr_disp32/mov_rm_disp32...")
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
     (displayln "  x64 combined arith...")
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
     (displayln "  x64 extended regs...")
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
   ))

(displayln "Running tests...")
(run-tests tests 'verbose)
(displayln "Done!")
