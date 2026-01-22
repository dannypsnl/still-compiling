#lang racket/base

;;; Racket FFI bindings for dynasm (aarch64 macOS)

(require ffi/unsafe
         ffi/unsafe/define)

;; Load the shared library
(define dynasm-lib
  (ffi-lib (build-path (current-directory) "libdynasm")))

(define-ffi-definer define-dynasm dynasm-lib)

;; ============================================
;; Core API
;; ============================================

;; Opaque pointer type for dynasm_buffer_t
(define _dynasm-buffer _pointer)

;; Create a new code buffer
(define-dynasm dynasm-create
  (_fun _size -> _dynasm-buffer)
  #:c-id dynasm_create)

;; Free a code buffer
(define-dynasm dynasm-free
  (_fun _dynasm-buffer -> _void)
  #:c-id dynasm_free)

;; Get current position
(define-dynasm dynasm-pos
  (_fun _dynasm-buffer -> _size)
  #:c-id dynasm_pos)

;; Emit 32-bit instruction
(define-dynasm dynasm-emit32
  (_fun _dynasm-buffer _uint32 -> _void)
  #:c-id dynasm_emit32)

;; Finalize and get function pointer
(define-dynasm dynasm-finalize
  (_fun _dynasm-buffer -> _pointer)
  #:c-id dynasm_finalize)

;; ============================================
;; AArch64 instruction encoders
;; ============================================

(define-dynasm aarch64-movz
  (_fun _int _uint16 _int -> _uint32)
  #:c-id aarch64_movz)

(define-dynasm aarch64-movk
  (_fun _int _uint16 _int -> _uint32)
  #:c-id aarch64_movk)

(define-dynasm aarch64-add-imm
  (_fun _int _int _uint16 -> _uint32)
  #:c-id aarch64_add_imm)

(define-dynasm aarch64-sub-imm
  (_fun _int _int _uint16 -> _uint32)
  #:c-id aarch64_sub_imm)

(define-dynasm aarch64-add-reg
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_add_reg)

(define-dynasm aarch64-sub-reg
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_sub_reg)

(define-dynasm aarch64-mul
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_mul)

(define-dynasm aarch64-sdiv
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_sdiv)

(define-dynasm aarch64-ret
  (_fun -> _uint32)
  #:c-id aarch64_ret)

(define-dynasm aarch64-ret-reg
  (_fun _int -> _uint32)
  #:c-id aarch64_ret_reg)

(define-dynasm aarch64-blr
  (_fun _int -> _uint32)
  #:c-id aarch64_blr)

(define-dynasm aarch64-br
  (_fun _int -> _uint32)
  #:c-id aarch64_br)

(define-dynasm aarch64-ldr-imm
  (_fun _int _int _uint16 -> _uint32)
  #:c-id aarch64_ldr_imm)

(define-dynasm aarch64-str-imm
  (_fun _int _int _uint16 -> _uint32)
  #:c-id aarch64_str_imm)

(define-dynasm aarch64-stp-pre
  (_fun _int _int _int _int16 -> _uint32)
  #:c-id aarch64_stp_pre)

(define-dynasm aarch64-ldp-post
  (_fun _int _int _int _int16 -> _uint32)
  #:c-id aarch64_ldp_post)

(define-dynasm aarch64-cmp-reg
  (_fun _int _int -> _uint32)
  #:c-id aarch64_cmp_reg)

(define-dynasm aarch64-cmp-imm
  (_fun _int _uint16 -> _uint32)
  #:c-id aarch64_cmp_imm)

(define-dynasm aarch64-b-cond
  (_fun _int _int32 -> _uint32)
  #:c-id aarch64_b_cond)

(define-dynasm aarch64-b
  (_fun _int32 -> _uint32)
  #:c-id aarch64_b)

(define-dynasm aarch64-nop
  (_fun -> _uint32)
  #:c-id aarch64_nop)

;; ============================================
;; Register constants
;; ============================================

(define X0  0)
(define X1  1)
(define X2  2)
(define X3  3)
(define X4  4)
(define X5  5)
(define X6  6)
(define X7  7)
(define X8  8)
(define X9  9)
(define X10 10)
(define X11 11)
(define X12 12)
(define X13 13)
(define X14 14)
(define X15 15)
(define X16 16)
(define X17 17)
(define X18 18)
(define X19 19)
(define X20 20)
(define X21 21)
(define X22 22)
(define X23 23)
(define X24 24)
(define X25 25)
(define X26 26)
(define X27 27)
(define X28 28)
(define X29 29)  ; Frame pointer
(define X30 30)  ; Link register
(define SP  31)  ; Stack pointer
(define XZR 31)  ; Zero register

(define FP X29)
(define LR X30)

;; Condition codes
(define COND-EQ #x0)
(define COND-NE #x1)
(define COND-GE #xA)
(define COND-LT #xB)
(define COND-GT #xC)
(define COND-LE #xD)

;; ============================================
;; High-level helpers
;; ============================================

;; Emit an instruction to the buffer
(define (emit! buf inst)
  (dynasm-emit32 buf inst))

;; Create a callable function from a finalized buffer
;; sig is the FFI function signature, e.g., (_fun _int64 _int64 -> _int64)
(define (make-jit-function buf sig)
  (cast (dynasm-finalize buf) _pointer sig))

;; ============================================
;; Exports
;; ============================================

(provide
 ;; Core API
 dynasm-create
 dynasm-free
 dynasm-pos
 dynasm-emit32
 dynasm-finalize

 ;; Instructions
 aarch64-movz
 aarch64-movk
 aarch64-add-imm
 aarch64-sub-imm
 aarch64-add-reg
 aarch64-sub-reg
 aarch64-mul
 aarch64-sdiv
 aarch64-ret
 aarch64-ret-reg
 aarch64-blr
 aarch64-br
 aarch64-ldr-imm
 aarch64-str-imm
 aarch64-stp-pre
 aarch64-ldp-post
 aarch64-cmp-reg
 aarch64-cmp-imm
 aarch64-b-cond
 aarch64-b
 aarch64-nop

 ;; Registers
 X0 X1 X2 X3 X4 X5 X6 X7 X8 X9
 X10 X11 X12 X13 X14 X15 X16 X17 X18 X19
 X20 X21 X22 X23 X24 X25 X26 X27 X28 X29 X30
 SP XZR FP LR

 ;; Conditions
 COND-EQ COND-NE COND-GE COND-LT COND-GT COND-LE

 ;; Helpers
 emit!
 make-jit-function)
