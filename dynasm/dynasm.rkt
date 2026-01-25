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
;; x64 instruction type and emitter
;; ============================================

;; x64_insn_t struct: 15 bytes + 1 byte length
(define-cstruct _x64-insn
  ([bytes (_array _uint8 15)]
   [len _uint8]))

;; Emit x64 instruction to buffer
(define-dynasm dynasm-emit-x64
  (_fun _dynasm-buffer _x64-insn -> _void)
  #:c-id dynasm_emit_x64)

;; ============================================
;; x64 instruction encoders
;; ============================================

;; MOV r64, imm64
(define-dynasm x64-mov-imm64
  (_fun _int _uint64 -> _x64-insn)
  #:c-id x64_mov_imm64)

;; MOV r64, imm32 (sign-extended)
(define-dynasm x64-mov-imm32
  (_fun _int _int32 -> _x64-insn)
  #:c-id x64_mov_imm32)

;; MOV r64, r64
(define-dynasm x64-mov-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_mov_reg)

;; MOV r64, [r64]
(define-dynasm x64-mov-rm
  (_fun _int _int -> _x64-insn)
  #:c-id x64_mov_rm)

;; MOV r64, [r64 + disp32]
(define-dynasm x64-mov-rm-disp32
  (_fun _int _int _int32 -> _x64-insn)
  #:c-id x64_mov_rm_disp32)

;; MOV [r64], r64
(define-dynasm x64-mov-mr
  (_fun _int _int -> _x64-insn)
  #:c-id x64_mov_mr)

;; MOV [r64 + disp32], r64
(define-dynasm x64-mov-mr-disp32
  (_fun _int _int32 _int -> _x64-insn)
  #:c-id x64_mov_mr_disp32)

;; ADD r64, r64
(define-dynasm x64-add-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_add_reg)

;; ADD r64, imm32
(define-dynasm x64-add-imm32
  (_fun _int _int32 -> _x64-insn)
  #:c-id x64_add_imm32)

;; ADD r64, imm8
(define-dynasm x64-add-imm8
  (_fun _int _int8 -> _x64-insn)
  #:c-id x64_add_imm8)

;; SUB r64, r64
(define-dynasm x64-sub-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_sub_reg)

;; SUB r64, imm32
(define-dynasm x64-sub-imm32
  (_fun _int _int32 -> _x64-insn)
  #:c-id x64_sub_imm32)

;; SUB r64, imm8
(define-dynasm x64-sub-imm8
  (_fun _int _int8 -> _x64-insn)
  #:c-id x64_sub_imm8)

;; IMUL r64, r64
(define-dynasm x64-imul-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_imul_reg)

;; IMUL r64, r64, imm32
(define-dynasm x64-imul-imm32
  (_fun _int _int _int32 -> _x64-insn)
  #:c-id x64_imul_imm32)

;; IDIV r64
(define-dynasm x64-idiv-reg
  (_fun _int -> _x64-insn)
  #:c-id x64_idiv_reg)

;; CQO
(define-dynasm x64-cqo
  (_fun -> _x64-insn)
  #:c-id x64_cqo)

;; SHL r64, imm8
(define-dynasm x64-shl-imm
  (_fun _int _uint8 -> _x64-insn)
  #:c-id x64_shl_imm)

;; SHL r64, CL
(define-dynasm x64-shl-cl
  (_fun _int -> _x64-insn)
  #:c-id x64_shl_cl)

;; SHR r64, imm8
(define-dynasm x64-shr-imm
  (_fun _int _uint8 -> _x64-insn)
  #:c-id x64_shr_imm)

;; SHR r64, CL
(define-dynasm x64-shr-cl
  (_fun _int -> _x64-insn)
  #:c-id x64_shr_cl)

;; SAR r64, imm8
(define-dynasm x64-sar-imm
  (_fun _int _uint8 -> _x64-insn)
  #:c-id x64_sar_imm)

;; SAR r64, CL
(define-dynasm x64-sar-cl
  (_fun _int -> _x64-insn)
  #:c-id x64_sar_cl)

;; CMP r64, r64
(define-dynasm x64-cmp-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_cmp_reg)

;; CMP r64, imm32
(define-dynasm x64-cmp-imm32
  (_fun _int _int32 -> _x64-insn)
  #:c-id x64_cmp_imm32)

;; CMP r64, imm8
(define-dynasm x64-cmp-imm8
  (_fun _int _int8 -> _x64-insn)
  #:c-id x64_cmp_imm8)

;; TEST r64, r64
(define-dynasm x64-test-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_test_reg)

;; JMP rel32
(define-dynasm x64-jmp-rel32
  (_fun _int32 -> _x64-insn)
  #:c-id x64_jmp_rel32)

;; JMP rel8
(define-dynasm x64-jmp-rel8
  (_fun _int8 -> _x64-insn)
  #:c-id x64_jmp_rel8)

;; Jcc rel32
(define-dynasm x64-jcc-rel32
  (_fun _int _int32 -> _x64-insn)
  #:c-id x64_jcc_rel32)

;; Jcc rel8
(define-dynasm x64-jcc-rel8
  (_fun _int _int8 -> _x64-insn)
  #:c-id x64_jcc_rel8)

;; CALL rel32
(define-dynasm x64-call-rel32
  (_fun _int32 -> _x64-insn)
  #:c-id x64_call_rel32)

;; RET
(define-dynasm x64-ret
  (_fun -> _x64-insn)
  #:c-id x64_ret)

;; PUSH r64
(define-dynasm x64-push
  (_fun _int -> _x64-insn)
  #:c-id x64_push)

;; POP r64
(define-dynasm x64-pop
  (_fun _int -> _x64-insn)
  #:c-id x64_pop)

;; NOP
(define-dynasm x64-nop
  (_fun -> _x64-insn)
  #:c-id x64_nop)

;; NEG r64
(define-dynasm x64-neg
  (_fun _int -> _x64-insn)
  #:c-id x64_neg)

;; AND r64, r64
(define-dynasm x64-and-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_and_reg)

;; AND r64, imm32
(define-dynasm x64-and-imm32
  (_fun _int _int32 -> _x64-insn)
  #:c-id x64_and_imm32)

;; OR r64, r64
(define-dynasm x64-or-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_or_reg)

;; XOR r64, r64
(define-dynasm x64-xor-reg
  (_fun _int _int -> _x64-insn)
  #:c-id x64_xor_reg)

;; INC r64
(define-dynasm x64-inc
  (_fun _int -> _x64-insn)
  #:c-id x64_inc)

;; DEC r64
(define-dynasm x64-dec
  (_fun _int -> _x64-insn)
  #:c-id x64_dec)

;; ============================================
;; RISC-V 64-bit instruction encoders (RV64I + RV64M)
;; ============================================

;; Arithmetic R-type
(define-dynasm riscv64-add
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_add)

(define-dynasm riscv64-sub
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_sub)

(define-dynasm riscv64-sll
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_sll)

(define-dynasm riscv64-slt
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_slt)

(define-dynasm riscv64-sltu
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_sltu)

(define-dynasm riscv64-xor
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_xor)

(define-dynasm riscv64-srl
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_srl)

(define-dynasm riscv64-sra
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_sra)

(define-dynasm riscv64-or
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_or)

(define-dynasm riscv64-and
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_and)

;; Arithmetic 64-bit word variants
(define-dynasm riscv64-addw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_addw)

(define-dynasm riscv64-subw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_subw)

(define-dynasm riscv64-sllw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_sllw)

(define-dynasm riscv64-srlw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_srlw)

(define-dynasm riscv64-sraw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_sraw)

;; Immediate arithmetic I-type
(define-dynasm riscv64-addi
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_addi)

(define-dynasm riscv64-slti
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_slti)

(define-dynasm riscv64-sltiu
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_sltiu)

(define-dynasm riscv64-xori
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_xori)

(define-dynasm riscv64-ori
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_ori)

(define-dynasm riscv64-andi
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_andi)

(define-dynasm riscv64-slli
  (_fun _int _int _uint32 -> _uint32)
  #:c-id riscv64_slli)

(define-dynasm riscv64-srli
  (_fun _int _int _uint32 -> _uint32)
  #:c-id riscv64_srli)

(define-dynasm riscv64-srai
  (_fun _int _int _uint32 -> _uint32)
  #:c-id riscv64_srai)

;; Immediate 64-bit word variants
(define-dynasm riscv64-addiw
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_addiw)

(define-dynasm riscv64-slliw
  (_fun _int _int _uint32 -> _uint32)
  #:c-id riscv64_slliw)

(define-dynasm riscv64-srliw
  (_fun _int _int _uint32 -> _uint32)
  #:c-id riscv64_srliw)

(define-dynasm riscv64-sraiw
  (_fun _int _int _uint32 -> _uint32)
  #:c-id riscv64_sraiw)

;; Load instructions
(define-dynasm riscv64-ld
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_ld)

(define-dynasm riscv64-lw
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_lw)

(define-dynasm riscv64-lwu
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_lwu)

(define-dynasm riscv64-lh
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_lh)

(define-dynasm riscv64-lhu
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_lhu)

(define-dynasm riscv64-lb
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_lb)

(define-dynasm riscv64-lbu
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_lbu)

;; Store instructions
(define-dynasm riscv64-sd
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_sd)

(define-dynasm riscv64-sw
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_sw)

(define-dynasm riscv64-sh
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_sh)

(define-dynasm riscv64-sb
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_sb)

;; Upper immediate
(define-dynasm riscv64-lui
  (_fun _int _uint32 -> _uint32)
  #:c-id riscv64_lui)

(define-dynasm riscv64-auipc
  (_fun _int _uint32 -> _uint32)
  #:c-id riscv64_auipc)

;; Branch instructions
(define-dynasm riscv64-beq
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_beq)

(define-dynasm riscv64-bne
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_bne)

(define-dynasm riscv64-blt
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_blt)

(define-dynasm riscv64-bge
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_bge)

(define-dynasm riscv64-bltu
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_bltu)

(define-dynasm riscv64-bgeu
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_bgeu)

;; Jump instructions
(define-dynasm riscv64-jal
  (_fun _int _int32 -> _uint32)
  #:c-id riscv64_jal)

(define-dynasm riscv64-jalr
  (_fun _int _int _int32 -> _uint32)
  #:c-id riscv64_jalr)

;; RV64M - Multiply/Divide extension
(define-dynasm riscv64-mul
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_mul)

(define-dynasm riscv64-mulh
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_mulh)

(define-dynasm riscv64-mulhsu
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_mulhsu)

(define-dynasm riscv64-mulhu
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_mulhu)

(define-dynasm riscv64-div
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_div)

(define-dynasm riscv64-divu
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_divu)

(define-dynasm riscv64-rem
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_rem)

(define-dynasm riscv64-remu
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_remu)

(define-dynasm riscv64-mulw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_mulw)

(define-dynasm riscv64-divw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_divw)

(define-dynasm riscv64-divuw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_divuw)

(define-dynasm riscv64-remw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_remw)

(define-dynasm riscv64-remuw
  (_fun _int _int _int -> _uint32)
  #:c-id riscv64_remuw)

;; Pseudo-instructions
(define-dynasm riscv64-nop
  (_fun -> _uint32)
  #:c-id riscv64_nop)

(define-dynasm riscv64-mv
  (_fun _int _int -> _uint32)
  #:c-id riscv64_mv)

(define-dynasm riscv64-not
  (_fun _int _int -> _uint32)
  #:c-id riscv64_not)

(define-dynasm riscv64-neg
  (_fun _int _int -> _uint32)
  #:c-id riscv64_neg)

(define-dynasm riscv64-li
  (_fun _int _int32 -> _uint32)
  #:c-id riscv64_li)

(define-dynasm riscv64-ret
  (_fun -> _uint32)
  #:c-id riscv64_ret)

(define-dynasm riscv64-jr
  (_fun _int -> _uint32)
  #:c-id riscv64_jr)

(define-dynasm riscv64-j
  (_fun _int32 -> _uint32)
  #:c-id riscv64_j)

;; ============================================
;; RISC-V 64-bit register constants
;; ============================================

(define RV-X0  0)   ; zero - hardwired zero
(define RV-X1  1)   ; ra - return address
(define RV-X2  2)   ; sp - stack pointer
(define RV-X3  3)   ; gp - global pointer
(define RV-X4  4)   ; tp - thread pointer
(define RV-X5  5)   ; t0 - temporary
(define RV-X6  6)   ; t1
(define RV-X7  7)   ; t2
(define RV-X8  8)   ; s0/fp - saved/frame pointer
(define RV-X9  9)   ; s1 - saved
(define RV-X10 10)  ; a0 - argument/return value
(define RV-X11 11)  ; a1 - argument/return value
(define RV-X12 12)  ; a2 - argument
(define RV-X13 13)  ; a3
(define RV-X14 14)  ; a4
(define RV-X15 15)  ; a5
(define RV-X16 16)  ; a6
(define RV-X17 17)  ; a7
(define RV-X18 18)  ; s2 - saved
(define RV-X19 19)  ; s3
(define RV-X20 20)  ; s4
(define RV-X21 21)  ; s5
(define RV-X22 22)  ; s6
(define RV-X23 23)  ; s7
(define RV-X24 24)  ; s8
(define RV-X25 25)  ; s9
(define RV-X26 26)  ; s10
(define RV-X27 27)  ; s11
(define RV-X28 28)  ; t3 - temporary
(define RV-X29 29)  ; t4
(define RV-X30 30)  ; t5
(define RV-X31 31)  ; t6

;; ABI name aliases
(define RV-ZERO RV-X0)
(define RV-RA   RV-X1)
(define RV-SP   RV-X2)
(define RV-GP   RV-X3)
(define RV-TP   RV-X4)
(define RV-T0   RV-X5)
(define RV-T1   RV-X6)
(define RV-T2   RV-X7)
(define RV-S0   RV-X8)
(define RV-FP   RV-X8)
(define RV-S1   RV-X9)
(define RV-A0   RV-X10)
(define RV-A1   RV-X11)
(define RV-A2   RV-X12)
(define RV-A3   RV-X13)
(define RV-A4   RV-X14)
(define RV-A5   RV-X15)
(define RV-A6   RV-X16)
(define RV-A7   RV-X17)
(define RV-S2   RV-X18)
(define RV-S3   RV-X19)
(define RV-S4   RV-X20)
(define RV-S5   RV-X21)
(define RV-S6   RV-X22)
(define RV-S7   RV-X23)
(define RV-S8   RV-X24)
(define RV-S9   RV-X25)
(define RV-S10  RV-X26)
(define RV-S11  RV-X27)
(define RV-T3   RV-X28)
(define RV-T4   RV-X29)
(define RV-T5   RV-X30)
(define RV-T6   RV-X31)

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

;; Emit an x64 instruction to the buffer
(define (emit-x64! buf insn)
  (dynasm-emit-x64 buf insn))

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

 ;; RISC-V 64-bit instruction encoders
 riscv64-add riscv64-sub riscv64-sll riscv64-slt riscv64-sltu
 riscv64-xor riscv64-srl riscv64-sra riscv64-or riscv64-and
 riscv64-addw riscv64-subw riscv64-sllw riscv64-srlw riscv64-sraw
 riscv64-addi riscv64-slti riscv64-sltiu riscv64-xori riscv64-ori riscv64-andi
 riscv64-slli riscv64-srli riscv64-srai
 riscv64-addiw riscv64-slliw riscv64-srliw riscv64-sraiw
 riscv64-ld riscv64-lw riscv64-lwu riscv64-lh riscv64-lhu riscv64-lb riscv64-lbu
 riscv64-sd riscv64-sw riscv64-sh riscv64-sb
 riscv64-lui riscv64-auipc
 riscv64-beq riscv64-bne riscv64-blt riscv64-bge riscv64-bltu riscv64-bgeu
 riscv64-jal riscv64-jalr
 riscv64-mul riscv64-mulh riscv64-mulhsu riscv64-mulhu
 riscv64-div riscv64-divu riscv64-rem riscv64-remu
 riscv64-mulw riscv64-divw riscv64-divuw riscv64-remw riscv64-remuw
 riscv64-nop riscv64-mv riscv64-not riscv64-neg riscv64-li
 riscv64-ret riscv64-jr riscv64-j

 ;; RISC-V 64-bit registers
 RV-X0 RV-X1 RV-X2 RV-X3 RV-X4 RV-X5 RV-X6 RV-X7
 RV-X8 RV-X9 RV-X10 RV-X11 RV-X12 RV-X13 RV-X14 RV-X15
 RV-X16 RV-X17 RV-X18 RV-X19 RV-X20 RV-X21 RV-X22 RV-X23
 RV-X24 RV-X25 RV-X26 RV-X27 RV-X28 RV-X29 RV-X30 RV-X31
 RV-ZERO RV-RA RV-SP RV-GP RV-TP
 RV-T0 RV-T1 RV-T2 RV-T3 RV-T4 RV-T5 RV-T6
 RV-S0 RV-S1 RV-S2 RV-S3 RV-S4 RV-S5 RV-S6 RV-S7 RV-S8 RV-S9 RV-S10 RV-S11
 RV-FP RV-A0 RV-A1 RV-A2 RV-A3 RV-A4 RV-A5 RV-A6 RV-A7

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
 emit-x64!
 dynasm-emit-x64
 make-jit-function)
