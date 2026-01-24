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
  (define versions '("2" "1" #f))

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

;; Mode types
(define UC_MODE_ARM 0)
(define UC_MODE_LITTLE_ENDIAN 0)

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
 UC_MODE_ARM
 UC_MODE_LITTLE_ENDIAN
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
 uc-reg-write-u64
 uc-reg-read-u64
 uc-reg-write-u128
 uc-reg-read-u128
 uc-map-memory
 uc-write-code
 uc-emulate)
