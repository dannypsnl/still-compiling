#lang racket/base

;;; Racket FFI bindings for Unicorn CPU emulator (ARM64)

(require ffi/unsafe
         ffi/unsafe/define)

;; Load the Unicorn shared library
;; Use pkg-config to find the library path, with fallbacks

;; Try to get library path from pkg-config
(define (pkg-config-libdir pkg)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define-values (proc stdout stdin stderr)
      (subprocess #f #f #f "/usr/bin/env" "pkg-config" "--variable=libdir" pkg))
    (close-output-port stdin)
    (define result (read-line stdout))
    (close-input-port stdout)
    (close-input-port stderr)
    (subprocess-wait proc)
    (if (and (string? result) (not (equal? result "")))
        result
        #f)))

(define (find-unicorn-lib)
  (define versions '("2" #f))

  ;; Build search paths: pkg-config result first, then standard names
  (define pkg-config-path (pkg-config-libdir "unicorn"))
  (define search-patterns
    (append
     (if pkg-config-path
         (list (build-path pkg-config-path "libunicorn"))
         '())
     '("unicorn" "libunicorn")))  ; Standard names (uses system library search)

  (for/or ([pattern search-patterns])
    (for/or ([version versions])
      (with-handlers ([exn:fail? (lambda (e) #f)])
        (ffi-lib (if (path? pattern) (path->string pattern) pattern)
                 (list version))))))

(define unicorn-lib
  (or (find-unicorn-lib)
      (error 'unicorn-lib "Could not load Unicorn library. Please install libunicorn-dev")))

(define-ffi-definer define-unicorn unicorn-lib)

;; ============================================
;; Constants
;; ============================================

;; Architecture types
(define UC_ARCH_ARM 1)
(define UC_ARCH_ARM64 2)
(define UC_ARCH_X86 4)
(define UC_ARCH_RISCV 9)

;; Mode types
(define UC_MODE_ARM 0)
(define UC_MODE_LITTLE_ENDIAN 0)
(define UC_MODE_64 8)  ; 64-bit mode for x86
(define UC_MODE_RISCV32 16)  ; 32-bit RISC-V
(define UC_MODE_RISCV64 32)  ; 64-bit RISC-V

;; Error codes
(define UC_ERR_OK 0)

;; Memory permissions
(define UC_PROT_NONE 0)
(define UC_PROT_READ 1)
(define UC_PROT_WRITE 2)
(define UC_PROT_EXEC 4)
(define UC_PROT_ALL 7)

;; ARM64 registers (from unicorn/arm64.h - Unicorn 2.x values)
(define UC_ARM64_REG_INVALID 0)
(define UC_ARM64_REG_X0 199)
(define UC_ARM64_REG_X1 200)
(define UC_ARM64_REG_X2 201)
(define UC_ARM64_REG_X3 202)
(define UC_ARM64_REG_X4 203)
(define UC_ARM64_REG_X5 204)
(define UC_ARM64_REG_X6 205)
(define UC_ARM64_REG_X7 206)
(define UC_ARM64_REG_X8 207)
(define UC_ARM64_REG_X9 208)
(define UC_ARM64_REG_X10 209)
(define UC_ARM64_REG_X11 210)
(define UC_ARM64_REG_X12 211)
(define UC_ARM64_REG_X13 212)
(define UC_ARM64_REG_X14 213)
(define UC_ARM64_REG_X15 214)
(define UC_ARM64_REG_X16 215)
(define UC_ARM64_REG_X17 216)
(define UC_ARM64_REG_X18 217)
(define UC_ARM64_REG_X19 218)
(define UC_ARM64_REG_X20 219)
(define UC_ARM64_REG_X21 220)
(define UC_ARM64_REG_X22 221)
(define UC_ARM64_REG_X23 222)
(define UC_ARM64_REG_X24 223)
(define UC_ARM64_REG_X25 224)
(define UC_ARM64_REG_X26 225)
(define UC_ARM64_REG_X27 226)
(define UC_ARM64_REG_X28 227)
(define UC_ARM64_REG_X29 1)   ; FP (special in Unicorn 2)
(define UC_ARM64_REG_X30 2)   ; LR (special in Unicorn 2)
(define UC_ARM64_REG_SP 4)
(define UC_ARM64_REG_PC 260)

;; NEON/SIMD registers (V0-V31)
(define UC_ARM64_REG_V0 228)
(define UC_ARM64_REG_V1 229)
(define UC_ARM64_REG_V2 230)
(define UC_ARM64_REG_V3 231)
(define UC_ARM64_REG_V4 232)
(define UC_ARM64_REG_V5 233)
(define UC_ARM64_REG_V6 234)
(define UC_ARM64_REG_V7 235)
(define UC_ARM64_REG_V8 236)
(define UC_ARM64_REG_V9 237)
(define UC_ARM64_REG_V10 238)
(define UC_ARM64_REG_V11 239)
(define UC_ARM64_REG_V12 240)
(define UC_ARM64_REG_V13 241)
(define UC_ARM64_REG_V14 242)
(define UC_ARM64_REG_V15 243)
(define UC_ARM64_REG_V16 244)
(define UC_ARM64_REG_V17 245)
(define UC_ARM64_REG_V18 246)
(define UC_ARM64_REG_V19 247)
(define UC_ARM64_REG_V20 248)
(define UC_ARM64_REG_V21 249)
(define UC_ARM64_REG_V22 250)
(define UC_ARM64_REG_V23 251)
(define UC_ARM64_REG_V24 252)
(define UC_ARM64_REG_V25 253)
(define UC_ARM64_REG_V26 254)
(define UC_ARM64_REG_V27 255)
(define UC_ARM64_REG_V28 256)
(define UC_ARM64_REG_V29 257)
(define UC_ARM64_REG_V30 258)
(define UC_ARM64_REG_V31 259)

;; X86-64 registers (from unicorn/x86.h - Unicorn 2.x values)
(define UC_X86_REG_INVALID 0)
(define UC_X86_REG_RAX 35)
(define UC_X86_REG_RBP 36)
(define UC_X86_REG_RBX 37)
(define UC_X86_REG_RCX 38)
(define UC_X86_REG_RDI 39)
(define UC_X86_REG_RDX 40)
(define UC_X86_REG_RIP 41)
(define UC_X86_REG_RSI 43)
(define UC_X86_REG_RSP 44)
(define UC_X86_REG_R8 106)
(define UC_X86_REG_R9 107)
(define UC_X86_REG_R10 108)
(define UC_X86_REG_R11 109)
(define UC_X86_REG_R12 110)
(define UC_X86_REG_R13 111)
(define UC_X86_REG_R14 112)
(define UC_X86_REG_R15 113)

;; RISC-V registers (from unicorn/riscv.h - Unicorn 2.x values)
(define UC_RISCV_REG_INVALID 0)

;; General purpose registers (X0-X31)
(define UC_RISCV_REG_X0 1)
(define UC_RISCV_REG_X1 2)
(define UC_RISCV_REG_X2 3)
(define UC_RISCV_REG_X3 4)
(define UC_RISCV_REG_X4 5)
(define UC_RISCV_REG_X5 6)
(define UC_RISCV_REG_X6 7)
(define UC_RISCV_REG_X7 8)
(define UC_RISCV_REG_X8 9)
(define UC_RISCV_REG_X9 10)
(define UC_RISCV_REG_X10 11)
(define UC_RISCV_REG_X11 12)
(define UC_RISCV_REG_X12 13)
(define UC_RISCV_REG_X13 14)
(define UC_RISCV_REG_X14 15)
(define UC_RISCV_REG_X15 16)
(define UC_RISCV_REG_X16 17)
(define UC_RISCV_REG_X17 18)
(define UC_RISCV_REG_X18 19)
(define UC_RISCV_REG_X19 20)
(define UC_RISCV_REG_X20 21)
(define UC_RISCV_REG_X21 22)
(define UC_RISCV_REG_X22 23)
(define UC_RISCV_REG_X23 24)
(define UC_RISCV_REG_X24 25)
(define UC_RISCV_REG_X25 26)
(define UC_RISCV_REG_X26 27)
(define UC_RISCV_REG_X27 28)
(define UC_RISCV_REG_X28 29)
(define UC_RISCV_REG_X29 30)
(define UC_RISCV_REG_X30 31)
(define UC_RISCV_REG_X31 32)

;; Floating-point registers (F0-F31)
(define UC_RISCV_REG_F0 33)
(define UC_RISCV_REG_F1 34)
(define UC_RISCV_REG_F2 35)
(define UC_RISCV_REG_F3 36)
(define UC_RISCV_REG_F4 37)
(define UC_RISCV_REG_F5 38)
(define UC_RISCV_REG_F6 39)
(define UC_RISCV_REG_F7 40)
(define UC_RISCV_REG_F8 41)
(define UC_RISCV_REG_F9 42)
(define UC_RISCV_REG_F10 43)
(define UC_RISCV_REG_F11 44)
(define UC_RISCV_REG_F12 45)
(define UC_RISCV_REG_F13 46)
(define UC_RISCV_REG_F14 47)
(define UC_RISCV_REG_F15 48)
(define UC_RISCV_REG_F16 49)
(define UC_RISCV_REG_F17 50)
(define UC_RISCV_REG_F18 51)
(define UC_RISCV_REG_F19 52)
(define UC_RISCV_REG_F20 53)
(define UC_RISCV_REG_F21 54)
(define UC_RISCV_REG_F22 55)
(define UC_RISCV_REG_F23 56)
(define UC_RISCV_REG_F24 57)
(define UC_RISCV_REG_F25 58)
(define UC_RISCV_REG_F26 59)
(define UC_RISCV_REG_F27 60)
(define UC_RISCV_REG_F28 61)
(define UC_RISCV_REG_F29 62)
(define UC_RISCV_REG_F30 63)
(define UC_RISCV_REG_F31 64)

(define UC_RISCV_REG_PC 65)

;; ABI name aliases for general purpose registers
(define UC_RISCV_REG_ZERO UC_RISCV_REG_X0)  ; zero
(define UC_RISCV_REG_RA UC_RISCV_REG_X1)    ; ra (return address)
(define UC_RISCV_REG_SP UC_RISCV_REG_X2)    ; sp (stack pointer)
(define UC_RISCV_REG_GP UC_RISCV_REG_X3)    ; gp (global pointer)
(define UC_RISCV_REG_TP UC_RISCV_REG_X4)    ; tp (thread pointer)
(define UC_RISCV_REG_T0 UC_RISCV_REG_X5)    ; t0 (temporary)
(define UC_RISCV_REG_T1 UC_RISCV_REG_X6)    ; t1
(define UC_RISCV_REG_T2 UC_RISCV_REG_X7)    ; t2
(define UC_RISCV_REG_S0 UC_RISCV_REG_X8)    ; s0/fp (saved/frame pointer)
(define UC_RISCV_REG_FP UC_RISCV_REG_X8)    ; fp (frame pointer)
(define UC_RISCV_REG_S1 UC_RISCV_REG_X9)    ; s1
(define UC_RISCV_REG_A0 UC_RISCV_REG_X10)   ; a0 (argument/return value)
(define UC_RISCV_REG_A1 UC_RISCV_REG_X11)   ; a1
(define UC_RISCV_REG_A2 UC_RISCV_REG_X12)   ; a2
(define UC_RISCV_REG_A3 UC_RISCV_REG_X13)   ; a3
(define UC_RISCV_REG_A4 UC_RISCV_REG_X14)   ; a4
(define UC_RISCV_REG_A5 UC_RISCV_REG_X15)   ; a5
(define UC_RISCV_REG_A6 UC_RISCV_REG_X16)   ; a6
(define UC_RISCV_REG_A7 UC_RISCV_REG_X17)   ; a7
(define UC_RISCV_REG_S2 UC_RISCV_REG_X18)   ; s2
(define UC_RISCV_REG_S3 UC_RISCV_REG_X19)   ; s3
(define UC_RISCV_REG_S4 UC_RISCV_REG_X20)   ; s4
(define UC_RISCV_REG_S5 UC_RISCV_REG_X21)   ; s5
(define UC_RISCV_REG_S6 UC_RISCV_REG_X22)   ; s6
(define UC_RISCV_REG_S7 UC_RISCV_REG_X23)   ; s7
(define UC_RISCV_REG_S8 UC_RISCV_REG_X24)   ; s8
(define UC_RISCV_REG_S9 UC_RISCV_REG_X25)   ; s9
(define UC_RISCV_REG_S10 UC_RISCV_REG_X26)  ; s10
(define UC_RISCV_REG_S11 UC_RISCV_REG_X27)  ; s11
(define UC_RISCV_REG_T3 UC_RISCV_REG_X28)   ; t3
(define UC_RISCV_REG_T4 UC_RISCV_REG_X29)   ; t4
(define UC_RISCV_REG_T5 UC_RISCV_REG_X30)   ; t5
(define UC_RISCV_REG_T6 UC_RISCV_REG_X31)   ; t6

;; ABI name aliases for floating-point registers
(define UC_RISCV_REG_FT0 UC_RISCV_REG_F0)   ; ft0
(define UC_RISCV_REG_FT1 UC_RISCV_REG_F1)   ; ft1
(define UC_RISCV_REG_FT2 UC_RISCV_REG_F2)   ; ft2
(define UC_RISCV_REG_FT3 UC_RISCV_REG_F3)   ; ft3
(define UC_RISCV_REG_FT4 UC_RISCV_REG_F4)   ; ft4
(define UC_RISCV_REG_FT5 UC_RISCV_REG_F5)   ; ft5
(define UC_RISCV_REG_FT6 UC_RISCV_REG_F6)   ; ft6
(define UC_RISCV_REG_FT7 UC_RISCV_REG_F7)   ; ft7
(define UC_RISCV_REG_FS0 UC_RISCV_REG_F8)   ; fs0
(define UC_RISCV_REG_FS1 UC_RISCV_REG_F9)   ; fs1
(define UC_RISCV_REG_FA0 UC_RISCV_REG_F10)  ; fa0
(define UC_RISCV_REG_FA1 UC_RISCV_REG_F11)  ; fa1
(define UC_RISCV_REG_FA2 UC_RISCV_REG_F12)  ; fa2
(define UC_RISCV_REG_FA3 UC_RISCV_REG_F13)  ; fa3
(define UC_RISCV_REG_FA4 UC_RISCV_REG_F14)  ; fa4
(define UC_RISCV_REG_FA5 UC_RISCV_REG_F15)  ; fa5
(define UC_RISCV_REG_FA6 UC_RISCV_REG_F16)  ; fa6
(define UC_RISCV_REG_FA7 UC_RISCV_REG_F17)  ; fa7
(define UC_RISCV_REG_FS2 UC_RISCV_REG_F18)  ; fs2
(define UC_RISCV_REG_FS3 UC_RISCV_REG_F19)  ; fs3
(define UC_RISCV_REG_FS4 UC_RISCV_REG_F20)  ; fs4
(define UC_RISCV_REG_FS5 UC_RISCV_REG_F21)  ; fs5
(define UC_RISCV_REG_FS6 UC_RISCV_REG_F22)  ; fs6
(define UC_RISCV_REG_FS7 UC_RISCV_REG_F23)  ; fs7
(define UC_RISCV_REG_FS8 UC_RISCV_REG_F24)  ; fs8
(define UC_RISCV_REG_FS9 UC_RISCV_REG_F25)  ; fs9
(define UC_RISCV_REG_FS10 UC_RISCV_REG_F26) ; fs10
(define UC_RISCV_REG_FS11 UC_RISCV_REG_F27) ; fs11
(define UC_RISCV_REG_FT8 UC_RISCV_REG_F28)  ; ft8
(define UC_RISCV_REG_FT9 UC_RISCV_REG_F29)  ; ft9
(define UC_RISCV_REG_FT10 UC_RISCV_REG_F30) ; ft10
(define UC_RISCV_REG_FT11 UC_RISCV_REG_F31) ; ft11

;; ============================================
;; Core API
;; ============================================

;; Opaque pointer type for uc_engine
(define _uc-engine (_cpointer 'uc-engine))

;; uc_open - Create a new emulator instance
;; uc_err uc_open(uc_arch arch, uc_mode mode, uc_engine **uc)
(define-unicorn uc-open-raw
  (_fun _int _int _pointer -> _int)
  #:c-id uc_open)

;; Wrapper to properly handle output pointer
(define (uc-open arch mode)
  (define uc-ptr (malloc _uc-engine 'atomic))
  (define err (uc-open-raw arch mode uc-ptr))
  (values err (ptr-ref uc-ptr _uc-engine)))

;; uc_close - Close and free an emulator instance
;; uc_err uc_close(uc_engine *uc)
(define-unicorn uc-close
  (_fun _uc-engine -> _int)
  #:c-id uc_close)

;; uc_mem_map - Map a memory region
;; uc_err uc_mem_map(uc_engine *uc, uint64_t address, size_t size, uint32_t perms)
(define-unicorn uc-mem-map
  (_fun _uc-engine _uint64 _size _uint32 -> _int)
  #:c-id uc_mem_map)

;; uc_mem_write - Write to memory
;; uc_err uc_mem_write(uc_engine *uc, uint64_t address, const void *bytes, size_t size)
(define-unicorn uc-mem-write
  (_fun _uc-engine _uint64 _bytes _size -> _int)
  #:c-id uc_mem_write)

;; uc_mem_read - Read from memory
;; uc_err uc_mem_read(uc_engine *uc, uint64_t address, void *bytes, size_t size)
(define-unicorn uc-mem-read
  (_fun _uc-engine _uint64 _bytes _size -> _int)
  #:c-id uc_mem_read)

;; uc_reg_write - Write to a register
;; uc_err uc_reg_write(uc_engine *uc, int regid, const void *value)
(define-unicorn uc-reg-write
  (_fun _uc-engine _int _pointer -> _int)
  #:c-id uc_reg_write)

;; uc_reg_read - Read from a register
;; uc_err uc_reg_read(uc_engine *uc, int regid, void *value)
(define-unicorn uc-reg-read
  (_fun _uc-engine _int _pointer -> _int)
  #:c-id uc_reg_read)

;; uc_emu_start - Emulate code
;; uc_err uc_emu_start(uc_engine *uc, uint64_t begin, uint64_t until, uint64_t timeout, size_t count)
(define-unicorn uc-emu-start
  (_fun _uc-engine _uint64 _uint64 _uint64 _size -> _int)
  #:c-id uc_emu_start)

;; uc_strerror - Get error message string
;; const char *uc_strerror(uc_err code)
(define-unicorn uc-strerror
  (_fun _int -> _string)
  #:c-id uc_strerror)

;; ============================================
;; Helper functions
;; ============================================

;; Create a new ARM64 emulator instance
(define (uc-create-arm64)
  (define-values (err uc) (uc-open UC_ARCH_ARM64 UC_MODE_ARM))
  (unless (= err UC_ERR_OK)
    (error 'uc-create-arm64 "Failed to create unicorn instance: ~a" (uc-strerror err)))
  uc)

;; Create a new x86-64 emulator instance
(define (uc-create-x64)
  (define-values (err uc) (uc-open UC_ARCH_X86 UC_MODE_64))
  (unless (= err UC_ERR_OK)
    (error 'uc-create-x64 "Failed to create unicorn instance: ~a" (uc-strerror err)))
  uc)

;; Create a new RISC-V 32-bit emulator instance
(define (uc-create-riscv32)
  (define-values (err uc) (uc-open UC_ARCH_RISCV UC_MODE_RISCV32))
  (unless (= err UC_ERR_OK)
    (error 'uc-create-riscv32 "Failed to create unicorn instance: ~a" (uc-strerror err)))
  uc)

;; Create a new RISC-V 64-bit emulator instance
(define (uc-create-riscv64)
  (define-values (err uc) (uc-open UC_ARCH_RISCV UC_MODE_RISCV64))
  (unless (= err UC_ERR_OK)
    (error 'uc-create-riscv64 "Failed to create unicorn instance: ~a" (uc-strerror err)))
  uc)

;; Write a 64-bit value to a register
(define (uc-reg-write-u64 uc regid value)
  (define buf (make-bytes 8))
  (integer->integer-bytes value 8 #f #f buf)
  (define err (uc-reg-write uc regid buf))
  (unless (= err UC_ERR_OK)
    (error 'uc-reg-write-u64 "Failed to write register: ~a" (uc-strerror err))))

;; Read a 64-bit value from a register
(define (uc-reg-read-u64 uc regid)
  (define buf (make-bytes 8))
  (define err (uc-reg-read uc regid buf))
  (unless (= err UC_ERR_OK)
    (error 'uc-reg-read-u64 "Failed to read register: ~a" (uc-strerror err)))
  (integer-bytes->integer buf #f #f))

;; Write a 32-bit value to a register
(define (uc-reg-write-u32 uc regid value)
  (define buf (make-bytes 4))
  (integer->integer-bytes value 4 #f #f buf)
  (define err (uc-reg-write uc regid buf))
  (unless (= err UC_ERR_OK)
    (error 'uc-reg-write-u32 "Failed to write register: ~a" (uc-strerror err))))

;; Read a 32-bit value from a register
(define (uc-reg-read-u32 uc regid)
  (define buf (make-bytes 4))
  (define err (uc-reg-read uc regid buf))
  (unless (= err UC_ERR_OK)
    (error 'uc-reg-read-u32 "Failed to read register: ~a" (uc-strerror err)))
  (integer-bytes->integer buf #f #f))

;; Write a 128-bit value to a SIMD register (V0-V31)
(define (uc-reg-write-u128 uc regid bytes)
  (unless (= (bytes-length bytes) 16)
    (error 'uc-reg-write-u128 "Expected 16 bytes for 128-bit value"))
  (define err (uc-reg-write uc regid bytes))
  (unless (= err UC_ERR_OK)
    (error 'uc-reg-write-u128 "Failed to write SIMD register: ~a" (uc-strerror err))))

;; Read a 128-bit value from a SIMD register (V0-V31)
(define (uc-reg-read-u128 uc regid)
  (define buf (make-bytes 16))
  (define err (uc-reg-read uc regid buf))
  (unless (= err UC_ERR_OK)
    (error 'uc-reg-read-u128 "Failed to read SIMD register: ~a" (uc-strerror err)))
  buf)

;; Map memory with read-write-execute permissions
(define (uc-map-memory uc address size)
  (define err (uc-mem-map uc address size UC_PROT_ALL))
  (unless (= err UC_ERR_OK)
    (error 'uc-map-memory "Failed to map memory: ~a" (uc-strerror err))))

;; Write code to memory
(define (uc-write-code uc address code-bytes)
  (define err (uc-mem-write uc address code-bytes (bytes-length code-bytes)))
  (unless (= err UC_ERR_OK)
    (error 'uc-write-code "Failed to write code: ~a" (uc-strerror err))))

;; Emulate code from start to end address
(define (uc-emulate uc start-addr end-addr)
  (define err (uc-emu-start uc start-addr end-addr 0 0))
  (unless (= err UC_ERR_OK)
    (error 'uc-emulate "Emulation failed: ~a" (uc-strerror err))))

;; ============================================
;; Exports
;; ============================================

(provide
 ;; Constants
 UC_ARCH_ARM64
 UC_ARCH_X86
 UC_ARCH_RISCV
 UC_MODE_ARM
 UC_MODE_LITTLE_ENDIAN
 UC_MODE_64
 UC_MODE_RISCV32
 UC_MODE_RISCV64
 UC_ERR_OK
 UC_PROT_NONE
 UC_PROT_READ
 UC_PROT_WRITE
 UC_PROT_EXEC
 UC_PROT_ALL

 ;; ARM64 GP registers
 UC_ARM64_REG_INVALID
 UC_ARM64_REG_X0 UC_ARM64_REG_X1 UC_ARM64_REG_X2 UC_ARM64_REG_X3
 UC_ARM64_REG_X4 UC_ARM64_REG_X5 UC_ARM64_REG_X6 UC_ARM64_REG_X7
 UC_ARM64_REG_X8 UC_ARM64_REG_X9 UC_ARM64_REG_X10 UC_ARM64_REG_X11
 UC_ARM64_REG_X12 UC_ARM64_REG_X13 UC_ARM64_REG_X14 UC_ARM64_REG_X15
 UC_ARM64_REG_X16 UC_ARM64_REG_X17 UC_ARM64_REG_X18 UC_ARM64_REG_X19
 UC_ARM64_REG_X20 UC_ARM64_REG_X21 UC_ARM64_REG_X22 UC_ARM64_REG_X23
 UC_ARM64_REG_X24 UC_ARM64_REG_X25 UC_ARM64_REG_X26 UC_ARM64_REG_X27
 UC_ARM64_REG_X28 UC_ARM64_REG_X29 UC_ARM64_REG_X30
 UC_ARM64_REG_SP
 UC_ARM64_REG_PC

 ;; ARM64 SIMD registers
 UC_ARM64_REG_V0 UC_ARM64_REG_V1 UC_ARM64_REG_V2 UC_ARM64_REG_V3
 UC_ARM64_REG_V4 UC_ARM64_REG_V5 UC_ARM64_REG_V6 UC_ARM64_REG_V7
 UC_ARM64_REG_V8 UC_ARM64_REG_V9 UC_ARM64_REG_V10 UC_ARM64_REG_V11
 UC_ARM64_REG_V12 UC_ARM64_REG_V13 UC_ARM64_REG_V14 UC_ARM64_REG_V15
 UC_ARM64_REG_V16 UC_ARM64_REG_V17 UC_ARM64_REG_V18 UC_ARM64_REG_V19
 UC_ARM64_REG_V20 UC_ARM64_REG_V21 UC_ARM64_REG_V22 UC_ARM64_REG_V23
 UC_ARM64_REG_V24 UC_ARM64_REG_V25 UC_ARM64_REG_V26 UC_ARM64_REG_V27
 UC_ARM64_REG_V28 UC_ARM64_REG_V29 UC_ARM64_REG_V30 UC_ARM64_REG_V31

 ;; X86-64 registers
 UC_X86_REG_INVALID
 UC_X86_REG_RAX UC_X86_REG_RCX UC_X86_REG_RDX UC_X86_REG_RBX
 UC_X86_REG_RSP UC_X86_REG_RBP UC_X86_REG_RSI UC_X86_REG_RDI
 UC_X86_REG_R8 UC_X86_REG_R9 UC_X86_REG_R10 UC_X86_REG_R11
 UC_X86_REG_R12 UC_X86_REG_R13 UC_X86_REG_R14 UC_X86_REG_R15
 UC_X86_REG_RIP

 ;; RISC-V GP registers (X0-X31)
 UC_RISCV_REG_INVALID
 UC_RISCV_REG_X0 UC_RISCV_REG_X1 UC_RISCV_REG_X2 UC_RISCV_REG_X3
 UC_RISCV_REG_X4 UC_RISCV_REG_X5 UC_RISCV_REG_X6 UC_RISCV_REG_X7
 UC_RISCV_REG_X8 UC_RISCV_REG_X9 UC_RISCV_REG_X10 UC_RISCV_REG_X11
 UC_RISCV_REG_X12 UC_RISCV_REG_X13 UC_RISCV_REG_X14 UC_RISCV_REG_X15
 UC_RISCV_REG_X16 UC_RISCV_REG_X17 UC_RISCV_REG_X18 UC_RISCV_REG_X19
 UC_RISCV_REG_X20 UC_RISCV_REG_X21 UC_RISCV_REG_X22 UC_RISCV_REG_X23
 UC_RISCV_REG_X24 UC_RISCV_REG_X25 UC_RISCV_REG_X26 UC_RISCV_REG_X27
 UC_RISCV_REG_X28 UC_RISCV_REG_X29 UC_RISCV_REG_X30 UC_RISCV_REG_X31

 ;; RISC-V FP registers (F0-F31)
 UC_RISCV_REG_F0 UC_RISCV_REG_F1 UC_RISCV_REG_F2 UC_RISCV_REG_F3
 UC_RISCV_REG_F4 UC_RISCV_REG_F5 UC_RISCV_REG_F6 UC_RISCV_REG_F7
 UC_RISCV_REG_F8 UC_RISCV_REG_F9 UC_RISCV_REG_F10 UC_RISCV_REG_F11
 UC_RISCV_REG_F12 UC_RISCV_REG_F13 UC_RISCV_REG_F14 UC_RISCV_REG_F15
 UC_RISCV_REG_F16 UC_RISCV_REG_F17 UC_RISCV_REG_F18 UC_RISCV_REG_F19
 UC_RISCV_REG_F20 UC_RISCV_REG_F21 UC_RISCV_REG_F22 UC_RISCV_REG_F23
 UC_RISCV_REG_F24 UC_RISCV_REG_F25 UC_RISCV_REG_F26 UC_RISCV_REG_F27
 UC_RISCV_REG_F28 UC_RISCV_REG_F29 UC_RISCV_REG_F30 UC_RISCV_REG_F31

 ;; RISC-V special registers
 UC_RISCV_REG_PC

 ;; RISC-V ABI name aliases (GP registers)
 UC_RISCV_REG_ZERO UC_RISCV_REG_RA UC_RISCV_REG_SP UC_RISCV_REG_GP
 UC_RISCV_REG_TP UC_RISCV_REG_T0 UC_RISCV_REG_T1 UC_RISCV_REG_T2
 UC_RISCV_REG_S0 UC_RISCV_REG_FP UC_RISCV_REG_S1
 UC_RISCV_REG_A0 UC_RISCV_REG_A1 UC_RISCV_REG_A2 UC_RISCV_REG_A3
 UC_RISCV_REG_A4 UC_RISCV_REG_A5 UC_RISCV_REG_A6 UC_RISCV_REG_A7
 UC_RISCV_REG_S2 UC_RISCV_REG_S3 UC_RISCV_REG_S4 UC_RISCV_REG_S5
 UC_RISCV_REG_S6 UC_RISCV_REG_S7 UC_RISCV_REG_S8 UC_RISCV_REG_S9
 UC_RISCV_REG_S10 UC_RISCV_REG_S11
 UC_RISCV_REG_T3 UC_RISCV_REG_T4 UC_RISCV_REG_T5 UC_RISCV_REG_T6

 ;; RISC-V ABI name aliases (FP registers)
 UC_RISCV_REG_FT0 UC_RISCV_REG_FT1 UC_RISCV_REG_FT2 UC_RISCV_REG_FT3
 UC_RISCV_REG_FT4 UC_RISCV_REG_FT5 UC_RISCV_REG_FT6 UC_RISCV_REG_FT7
 UC_RISCV_REG_FS0 UC_RISCV_REG_FS1
 UC_RISCV_REG_FA0 UC_RISCV_REG_FA1 UC_RISCV_REG_FA2 UC_RISCV_REG_FA3
 UC_RISCV_REG_FA4 UC_RISCV_REG_FA5 UC_RISCV_REG_FA6 UC_RISCV_REG_FA7
 UC_RISCV_REG_FS2 UC_RISCV_REG_FS3 UC_RISCV_REG_FS4 UC_RISCV_REG_FS5
 UC_RISCV_REG_FS6 UC_RISCV_REG_FS7 UC_RISCV_REG_FS8 UC_RISCV_REG_FS9
 UC_RISCV_REG_FS10 UC_RISCV_REG_FS11
 UC_RISCV_REG_FT8 UC_RISCV_REG_FT9 UC_RISCV_REG_FT10 UC_RISCV_REG_FT11

 ;; Core API
 uc-open
 uc-close
 uc-mem-map
 uc-mem-write
 uc-mem-read
 uc-reg-write
 uc-reg-read
 uc-emu-start
 uc-strerror

 ;; Helper functions
 uc-create-arm64
 uc-create-x64
 uc-create-riscv32
 uc-create-riscv64
 uc-reg-write-u64
 uc-reg-read-u64
 uc-reg-write-u32
 uc-reg-read-u32
 uc-reg-write-u128
 uc-reg-read-u128
 uc-map-memory
 uc-write-code
 uc-emulate)
