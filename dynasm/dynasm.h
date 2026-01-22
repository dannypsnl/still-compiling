#ifndef DYNASM_H
#define DYNASM_H

#include <stdint.h>
#include <stddef.h>

// Opaque handle for code buffer
typedef struct dynasm_buffer dynasm_buffer_t;

// Create a new code buffer with given capacity
dynasm_buffer_t* dynasm_create(size_t capacity);

// Free a code buffer
void dynasm_free(dynasm_buffer_t* buf);

// Get current code position (offset)
size_t dynasm_pos(dynasm_buffer_t* buf);

// Emit raw 32-bit instruction
void dynasm_emit32(dynasm_buffer_t* buf, uint32_t inst);

// Finalize and get executable function pointer
void* dynasm_finalize(dynasm_buffer_t* buf);

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
#define COND_EQ 0x0  // Equal
#define COND_NE 0x1  // Not equal
#define COND_GE 0xA  // Greater or equal (signed)
#define COND_LT 0xB  // Less than (signed)
#define COND_GT 0xC  // Greater than (signed)
#define COND_LE 0xD  // Less or equal (signed)

// B: unconditional branch (offset in instructions)
uint32_t aarch64_b(int32_t offset);

// NOP
uint32_t aarch64_nop(void);

// ============================================
// Register aliases
// ============================================
#define X0  0
#define X1  1
#define X2  2
#define X3  3
#define X4  4
#define X5  5
#define X6  6
#define X7  7
#define X8  8
#define X9  9
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
#define X29 29  // Frame pointer
#define X30 30  // Link register
#define SP  31  // Stack pointer (context dependent)
#define XZR 31  // Zero register (context dependent)

#define FP X29
#define LR X30

#endif // DYNASM_H
