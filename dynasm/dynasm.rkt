#lang racket/base

;;; Racket FFI bindings for dynasm (multi-architecture: AArch64, x64)

(require ffi/unsafe
         ffi/unsafe/define)

;; Load the shared library (handles .dylib on macOS, .so on Linux)
(define dynasm-lib
  (ffi-lib (build-path (current-directory) "libdynasm") '("" ".dylib" ".so")))

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

;; Emit 8-bit value
(define-dynasm dynasm-emit8
  (_fun _dynasm-buffer _uint8 -> _void)
  #:c-id dynasm_emit8)

;; Emit 16-bit value
(define-dynasm dynasm-emit16
  (_fun _dynasm-buffer _uint16 -> _void)
  #:c-id dynasm_emit16)

;; Emit 32-bit instruction
(define-dynasm dynasm-emit32
  (_fun _dynasm-buffer _uint32 -> _void)
  #:c-id dynasm_emit32)

;; Emit 64-bit value
(define-dynasm dynasm-emit64
  (_fun _dynasm-buffer _uint64 -> _void)
  #:c-id dynasm_emit64)

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

(define-dynasm aarch64-msub
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_msub)

(define-dynasm aarch64-lsr-imm
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_lsr_imm)

(define-dynasm aarch64-lsr-reg
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_lsr_reg)

(define-dynasm aarch64-lsl-imm
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_lsl_imm)

(define-dynasm aarch64-lsl-reg
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_lsl_reg)

(define-dynasm aarch64-and-imm
  (_fun _int _int _uint64 -> _uint32)
  #:c-id aarch64_and_imm)

(define-dynasm aarch64-tst-imm
  (_fun _int _uint64 -> _uint32)
  #:c-id aarch64_tst_imm)

(define-dynasm aarch64-clz
  (_fun _int _int -> _uint32)
  #:c-id aarch64_clz)

(define-dynasm aarch64-rbit
  (_fun _int _int -> _uint32)
  #:c-id aarch64_rbit)

(define-dynasm aarch64-csel
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_csel)

(define-dynasm aarch64-csneg
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_csneg)

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
;; AArch64 SIMD/NEON instruction encoders
;; ============================================

;; LDR (vector): load 128-bit vector
(define-dynasm aarch64-ldr-simd
  (_fun _int _int _uint16 -> _uint32)
  #:c-id aarch64_ldr_simd)

;; STR (vector): store 128-bit vector
(define-dynasm aarch64-str-simd
  (_fun _int _int _uint16 -> _uint32)
  #:c-id aarch64_str_simd)

;; DUP (element): duplicate element to all lanes
(define-dynasm aarch64-dup-element
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_dup_element)

;; DUP (general): duplicate GP register to vector
(define-dynasm aarch64-dup-general
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_dup_general)

;; ADD (vector, integer)
(define-dynasm aarch64-add-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_add_simd)

;; SUB (vector, integer)
(define-dynasm aarch64-sub-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_sub_simd)

;; MUL (vector, integer)
(define-dynasm aarch64-mul-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_mul_simd)

;; FADD (vector, float)
(define-dynasm aarch64-fadd-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_fadd_simd)

;; FSUB (vector, float)
(define-dynasm aarch64-fsub-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_fsub_simd)

;; FMUL (vector, float)
(define-dynasm aarch64-fmul-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_fmul_simd)

;; FDIV (vector, float)
(define-dynasm aarch64-fdiv-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_fdiv_simd)

;; ADDV (across vector)
(define-dynasm aarch64-addv
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_addv)

;; FADDP (pairwise add, float)
(define-dynasm aarch64-faddp-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_faddp_simd)

;; FMLA (fused multiply-add, float)
(define-dynasm aarch64-fmla-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_fmla_simd)

;; MLA (multiply-add, integer)
(define-dynasm aarch64-mla-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_mla_simd)

;; MOVI (move immediate to vector)
(define-dynasm aarch64-movi
  (_fun _int _uint8 _int -> _uint32)
  #:c-id aarch64_movi)

;; SCVTF (vector, convert signed int to float)
(define-dynasm aarch64-scvtf-simd
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_scvtf_simd)

;; FCVTZS (vector, convert float to signed int)
(define-dynasm aarch64-fcvtzs-simd
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_fcvtzs_simd)

;; SMAX (vector, signed max)
(define-dynasm aarch64-smax-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_smax_simd)

;; SMIN (vector, signed min)
(define-dynasm aarch64-smin-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_smin_simd)

;; FMAX (vector, float max)
(define-dynasm aarch64-fmax-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_fmax_simd)

;; FMIN (vector, float min)
(define-dynasm aarch64-fmin-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_fmin_simd)

;; SMAXV (across vector, signed max)
(define-dynasm aarch64-smaxv
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_smaxv)

;; SMINV (across vector, signed min)
(define-dynasm aarch64-sminv
  (_fun _int _int _int -> _uint32)
  #:c-id aarch64_sminv)

;; FMAXV (across vector, float max, 4S only)
(define-dynasm aarch64-fmaxv
  (_fun _int _int -> _uint32)
  #:c-id aarch64_fmaxv)

;; FMINV (across vector, float min, 4S only)
(define-dynasm aarch64-fminv
  (_fun _int _int -> _uint32)
  #:c-id aarch64_fminv)

;; FMOV (general to vector)
(define-dynasm aarch64-fmov-gp-to-vec
  (_fun _int _int -> _uint32)
  #:c-id aarch64_fmov_gp_to_vec)

;; FMOV (vector to general)
(define-dynasm aarch64-fmov-vec-to-gp
  (_fun _int _int -> _uint32)
  #:c-id aarch64_fmov_vec_to_gp)

;; UMOV (unsigned move from vector to GP)
(define-dynasm aarch64-umov
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_umov)

;; INS (general, insert GP into vector)
(define-dynasm aarch64-ins-general
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_ins_general)

;; EOR (vector, bitwise XOR)
(define-dynasm aarch64-eor-simd
  (_fun _int _int _int _int -> _uint32)
  #:c-id aarch64_eor_simd)

;; ============================================
;; x64 instruction encoders
;; ============================================

;; MOV r64, imm64
(define-dynasm x64-mov-imm64
  (_fun _dynasm-buffer _int _uint64 -> _void)
  #:c-id x64_mov_imm64)

;; MOV r64, imm32 (sign-extended)
(define-dynasm x64-mov-imm32
  (_fun _dynasm-buffer _int _int32 -> _void)
  #:c-id x64_mov_imm32)

;; MOV r64, r64
(define-dynasm x64-mov-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_mov_reg)

;; MOV r64, [r64]
(define-dynasm x64-mov-rm
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_mov_rm)

;; MOV r64, [r64 + disp32]
(define-dynasm x64-mov-rm-disp32
  (_fun _dynasm-buffer _int _int _int32 -> _void)
  #:c-id x64_mov_rm_disp32)

;; MOV [r64], r64
(define-dynasm x64-mov-mr
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_mov_mr)

;; MOV [r64 + disp32], r64
(define-dynasm x64-mov-mr-disp32
  (_fun _dynasm-buffer _int _int32 _int -> _void)
  #:c-id x64_mov_mr_disp32)

;; ADD r64, r64
(define-dynasm x64-add-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_add_reg)

;; ADD r64, imm32
(define-dynasm x64-add-imm32
  (_fun _dynasm-buffer _int _int32 -> _void)
  #:c-id x64_add_imm32)

;; ADD r64, imm8
(define-dynasm x64-add-imm8
  (_fun _dynasm-buffer _int _int8 -> _void)
  #:c-id x64_add_imm8)

;; SUB r64, r64
(define-dynasm x64-sub-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_sub_reg)

;; SUB r64, imm32
(define-dynasm x64-sub-imm32
  (_fun _dynasm-buffer _int _int32 -> _void)
  #:c-id x64_sub_imm32)

;; SUB r64, imm8
(define-dynasm x64-sub-imm8
  (_fun _dynasm-buffer _int _int8 -> _void)
  #:c-id x64_sub_imm8)

;; IMUL r64, r64
(define-dynasm x64-imul-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_imul_reg)

;; IMUL r64, r64, imm32
(define-dynasm x64-imul-imm32
  (_fun _dynasm-buffer _int _int _int32 -> _void)
  #:c-id x64_imul_imm32)

;; IDIV r64
(define-dynasm x64-idiv-reg
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_idiv_reg)

;; CQO
(define-dynasm x64-cqo
  (_fun _dynasm-buffer -> _void)
  #:c-id x64_cqo)

;; SHL r64, imm8
(define-dynasm x64-shl-imm
  (_fun _dynasm-buffer _int _uint8 -> _void)
  #:c-id x64_shl_imm)

;; SHL r64, CL
(define-dynasm x64-shl-cl
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_shl_cl)

;; SHR r64, imm8
(define-dynasm x64-shr-imm
  (_fun _dynasm-buffer _int _uint8 -> _void)
  #:c-id x64_shr_imm)

;; SHR r64, CL
(define-dynasm x64-shr-cl
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_shr_cl)

;; SAR r64, imm8
(define-dynasm x64-sar-imm
  (_fun _dynasm-buffer _int _uint8 -> _void)
  #:c-id x64_sar_imm)

;; SAR r64, CL
(define-dynasm x64-sar-cl
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_sar_cl)

;; CMP r64, r64
(define-dynasm x64-cmp-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_cmp_reg)

;; CMP r64, imm32
(define-dynasm x64-cmp-imm32
  (_fun _dynasm-buffer _int _int32 -> _void)
  #:c-id x64_cmp_imm32)

;; CMP r64, imm8
(define-dynasm x64-cmp-imm8
  (_fun _dynasm-buffer _int _int8 -> _void)
  #:c-id x64_cmp_imm8)

;; TEST r64, r64
(define-dynasm x64-test-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_test_reg)

;; JMP rel32
(define-dynasm x64-jmp-rel32
  (_fun _dynasm-buffer _int32 -> _void)
  #:c-id x64_jmp_rel32)

;; JMP rel8
(define-dynasm x64-jmp-rel8
  (_fun _dynasm-buffer _int8 -> _void)
  #:c-id x64_jmp_rel8)

;; Jcc rel32
(define-dynasm x64-jcc-rel32
  (_fun _dynasm-buffer _int _int32 -> _void)
  #:c-id x64_jcc_rel32)

;; Jcc rel8
(define-dynasm x64-jcc-rel8
  (_fun _dynasm-buffer _int _int8 -> _void)
  #:c-id x64_jcc_rel8)

;; CALL rel32
(define-dynasm x64-call-rel32
  (_fun _dynasm-buffer _int32 -> _void)
  #:c-id x64_call_rel32)

;; RET
(define-dynasm x64-ret
  (_fun _dynasm-buffer -> _void)
  #:c-id x64_ret)

;; PUSH r64
(define-dynasm x64-push
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_push)

;; POP r64
(define-dynasm x64-pop
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_pop)

;; NOP
(define-dynasm x64-nop
  (_fun _dynasm-buffer -> _void)
  #:c-id x64_nop)

;; NEG r64
(define-dynasm x64-neg
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_neg)

;; AND r64, r64
(define-dynasm x64-and-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_and_reg)

;; AND r64, imm32
(define-dynasm x64-and-imm32
  (_fun _dynasm-buffer _int _int32 -> _void)
  #:c-id x64_and_imm32)

;; OR r64, r64
(define-dynasm x64-or-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_or_reg)

;; XOR r64, r64
(define-dynasm x64-xor-reg
  (_fun _dynasm-buffer _int _int -> _void)
  #:c-id x64_xor_reg)

;; INC r64
(define-dynasm x64-inc
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_inc)

;; DEC r64
(define-dynasm x64-dec
  (_fun _dynasm-buffer _int -> _void)
  #:c-id x64_dec)

;; ============================================
;; AArch64 register constants
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

;; AArch64 SIMD/NEON registers (V0-V31)
(define V0  0)
(define V1  1)
(define V2  2)
(define V3  3)
(define V4  4)
(define V5  5)
(define V6  6)
(define V7  7)
(define V8  8)
(define V9  9)
(define V10 10)
(define V11 11)
(define V12 12)
(define V13 13)
(define V14 14)
(define V15 15)
(define V16 16)
(define V17 17)
(define V18 18)
(define V19 19)
(define V20 20)
(define V21 21)
(define V22 22)
(define V23 23)
(define V24 24)
(define V25 25)
(define V26 26)
(define V27 27)
(define V28 28)
(define V29 29)
(define V30 30)
(define V31 31)

;; AArch64 SIMD arrangement specifiers
(define SIMD-8B  0)  ; 8 x 8-bit (64-bit total)
(define SIMD-16B 1)  ; 16 x 8-bit (128-bit total)
(define SIMD-4H  2)  ; 4 x 16-bit (64-bit total)
(define SIMD-8H  3)  ; 8 x 16-bit (128-bit total)
(define SIMD-2S  4)  ; 2 x 32-bit (64-bit total)
(define SIMD-4S  5)  ; 4 x 32-bit (128-bit total)
(define SIMD-2D  6)  ; 2 x 64-bit (128-bit total)

;; AArch64 condition codes
(define COND-EQ #x0)
(define COND-NE #x1)
(define COND-GE #xA)
(define COND-LT #xB)
(define COND-GT #xC)
(define COND-LE #xD)

;; ============================================
;; x64 register constants
;; ============================================

(define RAX 0)
(define RCX 1)
(define RDX 2)
(define RBX 3)
(define RSP 4)
(define RBP 5)
(define RSI 6)
(define RDI 7)
(define R8 8)
(define R9 9)
(define R10 10)
(define R11 11)
(define R12 12)
(define R13 13)
(define R14 14)
(define R15 15)

;; ============================================
;; x64 condition codes
;; ============================================

(define CC-O #x0)   ; Overflow
(define CC-NO #x1)  ; No overflow
(define CC-B #x2)   ; Below (unsigned <)
(define CC-AE #x3)  ; Above or equal (unsigned >=)
(define CC-E #x4)   ; Equal
(define CC-NE #x5)  ; Not equal
(define CC-BE #x6)  ; Below or equal (unsigned <=)
(define CC-A #x7)   ; Above (unsigned >)
(define CC-S #x8)   ; Sign (negative)
(define CC-NS #x9)  ; No sign (non-negative)
(define CC-P #xA)   ; Parity even
(define CC-NP #xB)  ; Parity odd
(define CC-L #xC)   ; Less (signed <)
(define CC-GE #xD)  ; Greater or equal (signed >=)
(define CC-LE #xE)  ; Less or equal (signed <=)
(define CC-G #xF)   ; Greater (signed >)

;; x64 condition code aliases
(define CC-Z CC-E)   ; Zero (same as Equal)
(define CC-NZ CC-NE) ; Not zero (same as Not Equal)

;; ============================================
;; High-level helpers
;; ============================================

;; Emit an instruction to the buffer (for AArch64)
(define (emit! buf inst)
  (dynasm-emit32 buf inst))

;; Create a callable function from a finalized buffer
;; sig is the FFI function signature, e.g., (_fun _int64 _int64 -> _int64)
(define (make-jit-function buf sig)
  (define ptr (dynasm-finalize buf))
  (unless ptr
    (error 'make-jit-function "finalize failed, code instructions is NULL"))
  (cast ptr _pointer sig))

;; ============================================
;; Exports
;; ============================================

(provide
 ;; Core API
 dynasm-create
 dynasm-free
 dynasm-pos
 dynasm-emit8
 dynasm-emit16
 dynasm-emit32
 dynasm-emit64
 dynasm-finalize

 ;; AArch64 instructions
 aarch64-movz
 aarch64-movk
 aarch64-add-imm
 aarch64-sub-imm
 aarch64-add-reg
 aarch64-sub-reg
 aarch64-mul
 aarch64-sdiv
 aarch64-msub
 aarch64-lsr-imm
 aarch64-lsr-reg
 aarch64-lsl-imm
 aarch64-lsl-reg
 aarch64-and-imm
 aarch64-tst-imm
 aarch64-clz
 aarch64-rbit
 aarch64-csel
 aarch64-csneg
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

 ;; AArch64 SIMD instructions
 aarch64-ldr-simd
 aarch64-str-simd
 aarch64-dup-element
 aarch64-dup-general
 aarch64-add-simd
 aarch64-sub-simd
 aarch64-mul-simd
 aarch64-fadd-simd
 aarch64-fsub-simd
 aarch64-fmul-simd
 aarch64-fdiv-simd
 aarch64-addv
 aarch64-faddp-simd
 aarch64-fmla-simd
 aarch64-mla-simd
 aarch64-movi
 aarch64-scvtf-simd
 aarch64-fcvtzs-simd
 aarch64-smax-simd
 aarch64-smin-simd
 aarch64-fmax-simd
 aarch64-fmin-simd
 aarch64-smaxv
 aarch64-sminv
 aarch64-fmaxv
 aarch64-fminv
 aarch64-fmov-gp-to-vec
 aarch64-fmov-vec-to-gp
 aarch64-umov
 aarch64-ins-general
 aarch64-eor-simd

 ;; x64 instructions
 x64-mov-imm64
 x64-mov-imm32
 x64-mov-reg
 x64-mov-rm
 x64-mov-rm-disp32
 x64-mov-mr
 x64-mov-mr-disp32
 x64-add-reg
 x64-add-imm32
 x64-add-imm8
 x64-sub-reg
 x64-sub-imm32
 x64-sub-imm8
 x64-imul-reg
 x64-imul-imm32
 x64-idiv-reg
 x64-cqo
 x64-shl-imm
 x64-shl-cl
 x64-shr-imm
 x64-shr-cl
 x64-sar-imm
 x64-sar-cl
 x64-cmp-reg
 x64-cmp-imm32
 x64-cmp-imm8
 x64-test-reg
 x64-jmp-rel32
 x64-jmp-rel8
 x64-jcc-rel32
 x64-jcc-rel8
 x64-call-rel32
 x64-ret
 x64-push
 x64-pop
 x64-nop
 x64-neg
 x64-and-reg
 x64-and-imm32
 x64-or-reg
 x64-xor-reg
 x64-inc
 x64-dec

 ;; AArch64 general-purpose registers
 X0 X1 X2 X3 X4 X5 X6 X7 X8 X9
 X10 X11 X12 X13 X14 X15 X16 X17 X18 X19
 X20 X21 X22 X23 X24 X25 X26 X27 X28 X29 X30
 SP XZR FP LR

 ;; AArch64 SIMD registers
 V0 V1 V2 V3 V4 V5 V6 V7 V8 V9
 V10 V11 V12 V13 V14 V15 V16 V17 V18 V19
 V20 V21 V22 V23 V24 V25 V26 V27 V28 V29 V30 V31

 ;; AArch64 SIMD arrangement specifiers
 SIMD-8B SIMD-16B SIMD-4H SIMD-8H SIMD-2S SIMD-4S SIMD-2D

 ;; AArch64 conditions
 COND-EQ COND-NE COND-GE COND-LT COND-GT COND-LE

 ;; x64 registers
 RAX RCX RDX RBX RSP RBP RSI RDI
 R8 R9 R10 R11 R12 R13 R14 R15

 ;; x64 condition codes
 CC-O CC-NO CC-B CC-AE CC-E CC-NE CC-BE CC-A
 CC-S CC-NS CC-P CC-NP CC-L CC-GE CC-LE CC-G
 CC-Z CC-NZ

 ;; Helpers
 emit!
 make-jit-function)
