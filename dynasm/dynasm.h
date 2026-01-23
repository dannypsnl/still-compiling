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

#endif // DYNASM_H
