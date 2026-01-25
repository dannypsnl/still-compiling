#ifndef DYNASM_H
#define DYNASM_H

#include <stddef.h>
#include <stdint.h>

// Opaque handle for code buffer
typedef struct dynasm_buffer dynasm_buffer_t;

// Create a new code buffer with given capacity
dynasm_buffer_t *dynasm_create(size_t capacity);

// Free a code buffer
void dynasm_free(dynasm_buffer_t *buf);

// Get current code position (offset)
size_t dynasm_pos(dynasm_buffer_t *buf);

// Emit raw 32-bit instruction
void dynasm_emit32(dynasm_buffer_t *buf, uint32_t inst);

// Emit raw 8-bit value (for x64)
void dynasm_emit8(dynasm_buffer_t *buf, uint8_t val);

// Emit raw 16-bit value (for x64)
void dynasm_emit16(dynasm_buffer_t *buf, uint16_t val);

// Emit raw 64-bit value (for x64)
void dynasm_emit64(dynasm_buffer_t *buf, uint64_t val);

// Finalize and get executable function pointer
void *dynasm_finalize(dynasm_buffer_t *buf);

// ============================================
// AArch64 instruction encoders
// ============================================

// MOV immediate (MOVZ): rd = imm16 << (shift * 16)
// shift: 0, 1, 2, or 3 for 64-bit registers
uint32_t aarch64_movz(int rd, uint16_t imm16, int shift);

// MOVK: move immediate with keep (for building large constants)
uint32_t aarch64_movk(int rd, uint16_t imm16, int shift);

// ADD immediate: rd = rn + imm12
uint32_t aarch64_add_imm(int rd, int rn, uint16_t imm12);

// SUB immediate: rd = rn - imm12
uint32_t aarch64_sub_imm(int rd, int rn, uint16_t imm12);

// ADD register: rd = rn + rm
uint32_t aarch64_add_reg(int rd, int rn, int rm);

// SUB register: rd = rn - rm
uint32_t aarch64_sub_reg(int rd, int rn, int rm);

// MUL: rd = rn * rm
uint32_t aarch64_mul(int rd, int rn, int rm);

// SDIV: rd = rn / rm (signed)
uint32_t aarch64_sdiv(int rd, int rn, int rm);

// MSUB: rd = ra - rn * rm (multiply-subtract)
uint32_t aarch64_msub(int rd, int rn, int rm, int ra);

// LSR immediate: rd = rn >> imm6 (logical shift right)
uint32_t aarch64_lsr_imm(int rd, int rn, int imm6);

// LSR register: rd = rn >> (rm & 63) (logical shift right)
uint32_t aarch64_lsr_reg(int rd, int rn, int rm);

// LSL immediate: rd = rn << imm6 (logical shift left)
uint32_t aarch64_lsl_imm(int rd, int rn, int imm6);

// LSL register: rd = rn << (rm & 63) (logical shift left)
uint32_t aarch64_lsl_reg(int rd, int rn, int rm);

// AND immediate: rd = rn & imm (limited encoding)
uint32_t aarch64_and_imm(int rd, int rn, uint64_t imm);

// TST immediate: test rn & imm, set flags (alias of ANDS with XZR)
uint32_t aarch64_tst_imm(int rn, uint64_t imm);

// CLZ: rd = count leading zeros in rn
uint32_t aarch64_clz(int rd, int rn);

// RBIT: rd = reverse bits of rn
uint32_t aarch64_rbit(int rd, int rn);

// CSEL: rd = (cond) ? rn : rm
uint32_t aarch64_csel(int rd, int rn, int rm, int cond);

// CSNEG: rd = (cond) ? rn : -rm
uint32_t aarch64_csneg(int rd, int rn, int rm, int cond);

// RET: return from subroutine (uses x30/LR by default)
uint32_t aarch64_ret(void);

// RET with specific register
uint32_t aarch64_ret_reg(int rn);

// BLR: branch with link to register
uint32_t aarch64_blr(int rn);

// BR: branch to register
uint32_t aarch64_br(int rn);

// LDR immediate (unsigned offset): load 64-bit from [base + offset*8]
uint32_t aarch64_ldr_imm(int rt, int rn, uint16_t offset);

// STR immediate (unsigned offset): store 64-bit to [base + offset*8]
uint32_t aarch64_str_imm(int rt, int rn, uint16_t offset);

// STP: store pair of registers (pre-index)
uint32_t aarch64_stp_pre(int rt1, int rt2, int rn, int16_t offset);

// LDP: load pair of registers (post-index)
uint32_t aarch64_ldp_post(int rt1, int rt2, int rn, int16_t offset);

// CMP register: compare rn and rm
uint32_t aarch64_cmp_reg(int rn, int rm);

// CMP immediate: compare rn and imm12
uint32_t aarch64_cmp_imm(int rn, uint16_t imm12);

// B.cond: conditional branch (offset in instructions, not bytes)
uint32_t aarch64_b_cond(int cond, int32_t offset);

// Condition codes for B.cond
#define COND_EQ 0x0 // Equal
#define COND_NE 0x1 // Not equal
#define COND_GE 0xA // Greater or equal (signed)
#define COND_LT 0xB // Less than (signed)
#define COND_GT 0xC // Greater than (signed)
#define COND_LE 0xD // Less or equal (signed)

// B: unconditional branch (offset in instructions)
uint32_t aarch64_b(int32_t offset);

// NOP
uint32_t aarch64_nop(void);

// ============================================
// Register aliases
// ============================================
#define X0 0
#define X1 1
#define X2 2
#define X3 3
#define X4 4
#define X5 5
#define X6 6
#define X7 7
#define X8 8
#define X9 9
#define X10 10
#define X11 11
#define X12 12
#define X13 13
#define X14 14
#define X15 15
#define X16 16
#define X17 17
#define X18 18
#define X19 19
#define X20 20
#define X21 21
#define X22 22
#define X23 23
#define X24 24
#define X25 25
#define X26 26
#define X27 27
#define X28 28
#define X29 29 // Frame pointer
#define X30 30 // Link register
#define SP 31  // Stack pointer (context dependent)
#define XZR 31 // Zero register (context dependent)

#define FP X29
#define LR X30

// ============================================
// SIMD/NEON Vector registers (V0-V31)
// ============================================
#define V0 0
#define V1 1
#define V2 2
#define V3 3
#define V4 4
#define V5 5
#define V6 6
#define V7 7
#define V8 8
#define V9 9
#define V10 10
#define V11 11
#define V12 12
#define V13 13
#define V14 14
#define V15 15
#define V16 16
#define V17 17
#define V18 18
#define V19 19
#define V20 20
#define V21 21
#define V22 22
#define V23 23
#define V24 24
#define V25 25
#define V26 26
#define V27 27
#define V28 28
#define V29 29
#define V30 30
#define V31 31

// SIMD arrangement specifiers
#define SIMD_8B 0  // 8 x 8-bit (64-bit total)
#define SIMD_16B 1 // 16 x 8-bit (128-bit total)
#define SIMD_4H 2  // 4 x 16-bit (64-bit total)
#define SIMD_8H 3  // 8 x 16-bit (128-bit total)
#define SIMD_2S 4  // 2 x 32-bit (64-bit total)
#define SIMD_4S 5  // 4 x 32-bit (128-bit total)
#define SIMD_2D 6  // 2 x 64-bit (128-bit total)

// ============================================
// SIMD/NEON instruction encoders
// ============================================

// LDR (vector, immediate): load 128-bit vector from [base + offset*16]
uint32_t aarch64_ldr_simd(int vt, int rn, uint16_t offset);

// STR (vector, immediate): store 128-bit vector to [base + offset*16]
uint32_t aarch64_str_simd(int vt, int rn, uint16_t offset);

// DUP (element): duplicate scalar element to all lanes
// arr: arrangement (SIMD_4S, SIMD_2D, etc.)
uint32_t aarch64_dup_element(int vd, int vn, int arr, int index);

// DUP (general): duplicate general-purpose register to all vector lanes
uint32_t aarch64_dup_general(int vd, int rn, int arr);

// ADD (vector, integer): vd = vn + vm (element-wise)
uint32_t aarch64_add_simd(int vd, int vn, int vm, int arr);

// SUB (vector, integer): vd = vn - vm (element-wise)
uint32_t aarch64_sub_simd(int vd, int vn, int vm, int arr);

// MUL (vector, integer): vd = vn * vm (element-wise, not for 2D)
uint32_t aarch64_mul_simd(int vd, int vn, int vm, int arr);

// FADD (vector, float): vd = vn + vm (element-wise float)
// arr: SIMD_4S (4x float32) or SIMD_2D (2x float64)
uint32_t aarch64_fadd_simd(int vd, int vn, int vm, int arr);

// FSUB (vector, float): vd = vn - vm (element-wise float)
uint32_t aarch64_fsub_simd(int vd, int vn, int vm, int arr);

// FMUL (vector, float): vd = vn * vm (element-wise float)
uint32_t aarch64_fmul_simd(int vd, int vn, int vm, int arr);

// FDIV (vector, float): vd = vn / vm (element-wise float)
uint32_t aarch64_fdiv_simd(int vd, int vn, int vm, int arr);

// ADDV (across vector): reduce by adding all lanes
// Result in lowest lane of vd
uint32_t aarch64_addv(int vd, int vn, int arr);

// FADDP (pairwise add, float): add adjacent pairs
uint32_t aarch64_faddp_simd(int vd, int vn, int vm, int arr);

// FMLA (vector, float): vd = vd + vn * vm (fused multiply-add)
uint32_t aarch64_fmla_simd(int vd, int vn, int vm, int arr);

// MLA (vector, integer): vd = vd + vn * vm (multiply-add, not for 2D)
uint32_t aarch64_mla_simd(int vd, int vn, int vm, int arr);

// MOVI: move immediate to vector (set all lanes to imm8)
uint32_t aarch64_movi(int vd, uint8_t imm8, int arr);

// SCVTF (vector): convert signed integer to float
uint32_t aarch64_scvtf_simd(int vd, int vn, int arr);

// FCVTZS (vector): convert float to signed integer (truncate toward zero)
uint32_t aarch64_fcvtzs_simd(int vd, int vn, int arr);

// SMAX (vector): signed maximum element-wise
uint32_t aarch64_smax_simd(int vd, int vn, int vm, int arr);

// SMIN (vector): signed minimum element-wise
uint32_t aarch64_smin_simd(int vd, int vn, int vm, int arr);

// FMAX (vector): floating-point maximum element-wise
uint32_t aarch64_fmax_simd(int vd, int vn, int vm, int arr);

// FMIN (vector): floating-point minimum element-wise
uint32_t aarch64_fmin_simd(int vd, int vn, int vm, int arr);

// SMAXV (across vector): signed maximum across vector
uint32_t aarch64_smaxv(int vd, int vn, int arr);

// SMINV (across vector): signed minimum across vector
uint32_t aarch64_sminv(int vd, int vn, int arr);

// FMAXV (across vector): floating-point maximum across vector (4S only)
uint32_t aarch64_fmaxv(int vd, int vn);

// FMINV (across vector): floating-point minimum across vector (4S only)
uint32_t aarch64_fminv(int vd, int vn);

// FMOV (general to vector): move GP register to vector element 0
// For 64-bit: moves X register to D0 (double) lane
uint32_t aarch64_fmov_gp_to_vec(int vd, int rn);

// FMOV (vector to general): move vector element 0 to GP register
uint32_t aarch64_fmov_vec_to_gp(int rd, int vn);

// UMOV (unsigned move): extract unsigned integer from vector lane to GP
// register For 4S (32-bit lanes): extracts lane to W register (zero-extended to
// X)
uint32_t aarch64_umov(int rd, int vn, int arr, int index);

// INS (general): insert GP register into vector lane
uint32_t aarch64_ins_general(int vd, int rn, int arr, int index);

// EOR (vector): bitwise XOR, useful for zeroing registers (EOR Vd, Vn, Vn)
uint32_t aarch64_eor_simd(int vd, int vn, int vm, int arr);

// ============================================
// x64 (x86-64) register constants
// ============================================
#define RAX 0
#define RCX 1
#define RDX 2
#define RBX 3
#define RSP 4
#define RBP 5
#define RSI 6
#define RDI 7
#define R8 8
#define R9 9
#define R10 10
#define R11 11
#define R12 12
#define R13 13
#define R14 14
#define R15 15

// x64 instruction type (variable-length encoding)
typedef struct {
  uint8_t bytes[15]; // Max x64 instruction is 15 bytes
  uint8_t len;
} x64_insn_t;

// Emit x64 instruction to buffer
void dynasm_emit_x64(dynasm_buffer_t *buf, x64_insn_t insn);

// x64 condition codes for Jcc
#define X64_CC_O 0x0   // Overflow
#define X64_CC_NO 0x1  // No overflow
#define X64_CC_B 0x2   // Below (unsigned <)
#define X64_CC_AE 0x3  // Above or equal (unsigned >=)
#define X64_CC_E 0x4   // Equal
#define X64_CC_NE 0x5  // Not equal
#define X64_CC_BE 0x6  // Below or equal (unsigned <=)
#define X64_CC_A 0x7   // Above (unsigned >)
#define X64_CC_S 0x8   // Sign (negative)
#define X64_CC_NS 0x9  // No sign (non-negative)
#define X64_CC_P 0xA   // Parity even
#define X64_CC_NP 0xB  // Parity odd
#define X64_CC_L 0xC   // Less (signed <)
#define X64_CC_GE 0xD  // Greater or equal (signed >=)
#define X64_CC_LE 0xE  // Less or equal (signed <=)
#define X64_CC_G 0xF   // Greater (signed >)

// ============================================
// x64 instruction encoders
// ============================================

// MOV r64, imm64 (10 bytes: REX.W + B8+rd + imm64)
x64_insn_t x64_mov_imm64(int rd, uint64_t imm64);

// MOV r64, imm32 (sign-extended, 7 bytes: REX.W + C7 /0 + imm32)
x64_insn_t x64_mov_imm32(int rd, int32_t imm32);

// MOV r64, r64 (3 bytes: REX.W + 89 + ModRM)
x64_insn_t x64_mov_reg(int rd, int rs);

// MOV r64, [r64] (memory to register, base only)
x64_insn_t x64_mov_rm(int rd, int base);

// MOV r64, [r64 + disp32] (memory to register with displacement)
x64_insn_t x64_mov_rm_disp32(int rd, int base, int32_t disp);

// MOV [r64], r64 (register to memory, base only)
x64_insn_t x64_mov_mr(int base, int rs);

// MOV [r64 + disp32], r64 (register to memory with displacement)
x64_insn_t x64_mov_mr_disp32(int base, int32_t disp, int rs);

// ADD r64, r64
x64_insn_t x64_add_reg(int rd, int rs);

// ADD r64, imm32 (sign-extended)
x64_insn_t x64_add_imm32(int rd, int32_t imm);

// ADD r64, imm8 (sign-extended)
x64_insn_t x64_add_imm8(int rd, int8_t imm);

// SUB r64, r64
x64_insn_t x64_sub_reg(int rd, int rs);

// SUB r64, imm32 (sign-extended)
x64_insn_t x64_sub_imm32(int rd, int32_t imm);

// SUB r64, imm8 (sign-extended)
x64_insn_t x64_sub_imm8(int rd, int8_t imm);

// IMUL r64, r64 (signed multiply)
x64_insn_t x64_imul_reg(int rd, int rs);

// IMUL r64, r64, imm32 (signed multiply with immediate)
x64_insn_t x64_imul_imm32(int rd, int rs, int32_t imm);

// IDIV r64 (signed divide RDX:RAX by r64, quotient in RAX, remainder in RDX)
x64_insn_t x64_idiv_reg(int rs);

// CQO (sign-extend RAX into RDX:RAX)
x64_insn_t x64_cqo(void);

// SHL r64, imm8
x64_insn_t x64_shl_imm(int rd, uint8_t imm);

// SHL r64, CL
x64_insn_t x64_shl_cl(int rd);

// SHR r64, imm8
x64_insn_t x64_shr_imm(int rd, uint8_t imm);

// SHR r64, CL
x64_insn_t x64_shr_cl(int rd);

// SAR r64, imm8 (arithmetic shift right)
x64_insn_t x64_sar_imm(int rd, uint8_t imm);

// SAR r64, CL
x64_insn_t x64_sar_cl(int rd);

// CMP r64, r64
x64_insn_t x64_cmp_reg(int r1, int r2);

// CMP r64, imm32 (sign-extended)
x64_insn_t x64_cmp_imm32(int rd, int32_t imm);

// CMP r64, imm8 (sign-extended)
x64_insn_t x64_cmp_imm8(int rd, int8_t imm);

// TEST r64, r64
x64_insn_t x64_test_reg(int r1, int r2);

// JMP rel32 (5 bytes)
x64_insn_t x64_jmp_rel32(int32_t rel);

// JMP rel8 (2 bytes)
x64_insn_t x64_jmp_rel8(int8_t rel);

// Jcc rel32 (6 bytes: 0F 8x + rel32)
x64_insn_t x64_jcc_rel32(int cc, int32_t rel);

// Jcc rel8 (2 bytes: 7x + rel8)
x64_insn_t x64_jcc_rel8(int cc, int8_t rel);

// CALL rel32 (5 bytes)
x64_insn_t x64_call_rel32(int32_t rel);

// RET (1 byte)
x64_insn_t x64_ret(void);

// PUSH r64 (1-2 bytes)
x64_insn_t x64_push(int reg);

// POP r64 (1-2 bytes)
x64_insn_t x64_pop(int reg);

// NOP (1 byte)
x64_insn_t x64_nop(void);

// NEG r64 (two's complement negate)
x64_insn_t x64_neg(int rd);

// AND r64, r64
x64_insn_t x64_and_reg(int rd, int rs);

// AND r64, imm32
x64_insn_t x64_and_imm32(int rd, int32_t imm);

// OR r64, r64
x64_insn_t x64_or_reg(int rd, int rs);

// XOR r64, r64
x64_insn_t x64_xor_reg(int rd, int rs);

// INC r64
x64_insn_t x64_inc(int rd);

// DEC r64
x64_insn_t x64_dec(int rd);

// ============================================
// RISC-V 64-bit register constants
// ============================================
#define RV_X0  0   // zero - hardwired zero
#define RV_X1  1   // ra - return address
#define RV_X2  2   // sp - stack pointer
#define RV_X3  3   // gp - global pointer
#define RV_X4  4   // tp - thread pointer
#define RV_X5  5   // t0 - temporary
#define RV_X6  6   // t1
#define RV_X7  7   // t2
#define RV_X8  8   // s0/fp - saved/frame pointer
#define RV_X9  9   // s1 - saved
#define RV_X10 10  // a0 - argument/return value
#define RV_X11 11  // a1 - argument/return value
#define RV_X12 12  // a2 - argument
#define RV_X13 13  // a3
#define RV_X14 14  // a4
#define RV_X15 15  // a5
#define RV_X16 16  // a6
#define RV_X17 17  // a7
#define RV_X18 18  // s2 - saved
#define RV_X19 19  // s3
#define RV_X20 20  // s4
#define RV_X21 21  // s5
#define RV_X22 22  // s6
#define RV_X23 23  // s7
#define RV_X24 24  // s8
#define RV_X25 25  // s9
#define RV_X26 26  // s10
#define RV_X27 27  // s11
#define RV_X28 28  // t3 - temporary
#define RV_X29 29  // t4
#define RV_X30 30  // t5
#define RV_X31 31  // t6

// ABI name aliases
#define RV_ZERO RV_X0
#define RV_RA   RV_X1
#define RV_SP   RV_X2
#define RV_GP   RV_X3
#define RV_TP   RV_X4
#define RV_T0   RV_X5
#define RV_T1   RV_X6
#define RV_T2   RV_X7
#define RV_S0   RV_X8
#define RV_FP   RV_X8
#define RV_S1   RV_X9
#define RV_A0   RV_X10
#define RV_A1   RV_X11
#define RV_A2   RV_X12
#define RV_A3   RV_X13
#define RV_A4   RV_X14
#define RV_A5   RV_X15
#define RV_A6   RV_X16
#define RV_A7   RV_X17
#define RV_S2   RV_X18
#define RV_S3   RV_X19
#define RV_S4   RV_X20
#define RV_S5   RV_X21
#define RV_S6   RV_X22
#define RV_S7   RV_X23
#define RV_S8   RV_X24
#define RV_S9   RV_X25
#define RV_S10  RV_X26
#define RV_S11  RV_X27
#define RV_T3   RV_X28
#define RV_T4   RV_X29
#define RV_T5   RV_X30
#define RV_T6   RV_X31

// ============================================
// RISC-V 64-bit instruction encoders (RV64I + RV64M)
// ============================================

// Arithmetic R-type
uint32_t riscv64_add(int rd, int rs1, int rs2);
uint32_t riscv64_sub(int rd, int rs1, int rs2);
uint32_t riscv64_sll(int rd, int rs1, int rs2);
uint32_t riscv64_slt(int rd, int rs1, int rs2);
uint32_t riscv64_sltu(int rd, int rs1, int rs2);
uint32_t riscv64_xor(int rd, int rs1, int rs2);
uint32_t riscv64_srl(int rd, int rs1, int rs2);
uint32_t riscv64_sra(int rd, int rs1, int rs2);
uint32_t riscv64_or(int rd, int rs1, int rs2);
uint32_t riscv64_and(int rd, int rs1, int rs2);

// Arithmetic 64-bit word variants
uint32_t riscv64_addw(int rd, int rs1, int rs2);
uint32_t riscv64_subw(int rd, int rs1, int rs2);
uint32_t riscv64_sllw(int rd, int rs1, int rs2);
uint32_t riscv64_srlw(int rd, int rs1, int rs2);
uint32_t riscv64_sraw(int rd, int rs1, int rs2);

// Immediate arithmetic I-type
uint32_t riscv64_addi(int rd, int rs1, int32_t imm);
uint32_t riscv64_slti(int rd, int rs1, int32_t imm);
uint32_t riscv64_sltiu(int rd, int rs1, int32_t imm);
uint32_t riscv64_xori(int rd, int rs1, int32_t imm);
uint32_t riscv64_ori(int rd, int rs1, int32_t imm);
uint32_t riscv64_andi(int rd, int rs1, int32_t imm);
uint32_t riscv64_slli(int rd, int rs1, uint32_t shamt);
uint32_t riscv64_srli(int rd, int rs1, uint32_t shamt);
uint32_t riscv64_srai(int rd, int rs1, uint32_t shamt);

// Immediate 64-bit word variants
uint32_t riscv64_addiw(int rd, int rs1, int32_t imm);
uint32_t riscv64_slliw(int rd, int rs1, uint32_t shamt);
uint32_t riscv64_srliw(int rd, int rs1, uint32_t shamt);
uint32_t riscv64_sraiw(int rd, int rs1, uint32_t shamt);

// Load instructions
uint32_t riscv64_ld(int rd, int rs1, int32_t offset);
uint32_t riscv64_lw(int rd, int rs1, int32_t offset);
uint32_t riscv64_lwu(int rd, int rs1, int32_t offset);
uint32_t riscv64_lh(int rd, int rs1, int32_t offset);
uint32_t riscv64_lhu(int rd, int rs1, int32_t offset);
uint32_t riscv64_lb(int rd, int rs1, int32_t offset);
uint32_t riscv64_lbu(int rd, int rs1, int32_t offset);

// Store instructions
uint32_t riscv64_sd(int rs2, int rs1, int32_t offset);
uint32_t riscv64_sw(int rs2, int rs1, int32_t offset);
uint32_t riscv64_sh(int rs2, int rs1, int32_t offset);
uint32_t riscv64_sb(int rs2, int rs1, int32_t offset);

// Upper immediate
uint32_t riscv64_lui(int rd, uint32_t imm);
uint32_t riscv64_auipc(int rd, uint32_t imm);

// Branch instructions
uint32_t riscv64_beq(int rs1, int rs2, int32_t offset);
uint32_t riscv64_bne(int rs1, int rs2, int32_t offset);
uint32_t riscv64_blt(int rs1, int rs2, int32_t offset);
uint32_t riscv64_bge(int rs1, int rs2, int32_t offset);
uint32_t riscv64_bltu(int rs1, int rs2, int32_t offset);
uint32_t riscv64_bgeu(int rs1, int rs2, int32_t offset);

// Jump instructions
uint32_t riscv64_jal(int rd, int32_t offset);
uint32_t riscv64_jalr(int rd, int rs1, int32_t imm);

// RV64M - Multiply/Divide extension
uint32_t riscv64_mul(int rd, int rs1, int rs2);
uint32_t riscv64_mulh(int rd, int rs1, int rs2);
uint32_t riscv64_mulhsu(int rd, int rs1, int rs2);
uint32_t riscv64_mulhu(int rd, int rs1, int rs2);
uint32_t riscv64_div(int rd, int rs1, int rs2);
uint32_t riscv64_divu(int rd, int rs1, int rs2);
uint32_t riscv64_rem(int rd, int rs1, int rs2);
uint32_t riscv64_remu(int rd, int rs1, int rs2);
uint32_t riscv64_mulw(int rd, int rs1, int rs2);
uint32_t riscv64_divw(int rd, int rs1, int rs2);
uint32_t riscv64_divuw(int rd, int rs1, int rs2);
uint32_t riscv64_remw(int rd, int rs1, int rs2);
uint32_t riscv64_remuw(int rd, int rs1, int rs2);

// Pseudo-instructions
uint32_t riscv64_nop(void);
uint32_t riscv64_mv(int rd, int rs);
uint32_t riscv64_not(int rd, int rs);
uint32_t riscv64_neg(int rd, int rs);
uint32_t riscv64_li(int rd, int32_t imm);
uint32_t riscv64_ret(void);
uint32_t riscv64_jr(int rs);
uint32_t riscv64_j(int32_t offset);

#endif // DYNASM_H
