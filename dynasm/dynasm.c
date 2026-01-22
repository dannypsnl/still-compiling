#include "dynasm.h"
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#ifdef __APPLE__
#include <libkern/OSCacheControl.h>
#include <pthread.h>
#endif

struct dynasm_buffer {
    uint8_t* code;
    size_t capacity;
    size_t size;
    int finalized;
};

dynasm_buffer_t* dynasm_create(size_t capacity) {
    dynasm_buffer_t* buf = malloc(sizeof(dynasm_buffer_t));
    if (!buf) return NULL;

    // Allocate with MAP_JIT on macOS for W^X compliance
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;
#ifdef __APPLE__
    flags |= MAP_JIT;
#endif

    buf->code = mmap(NULL, capacity,
                     PROT_READ | PROT_WRITE | PROT_EXEC,
                     flags, -1, 0);

    if (buf->code == MAP_FAILED) {
        free(buf);
        return NULL;
    }

    buf->capacity = capacity;
    buf->size = 0;
    buf->finalized = 0;

    return buf;
}

void dynasm_free(dynasm_buffer_t* buf) {
    if (buf) {
        if (buf->code && buf->code != MAP_FAILED) {
            munmap(buf->code, buf->capacity);
        }
        free(buf);
    }
}

size_t dynasm_pos(dynasm_buffer_t* buf) {
    return buf->size;
}

void dynasm_emit32(dynasm_buffer_t* buf, uint32_t inst) {
    if (buf->size + 4 > buf->capacity) return;

#ifdef __APPLE__
    // Enable write access on Apple Silicon
    pthread_jit_write_protect_np(0);
#endif

    memcpy(buf->code + buf->size, &inst, 4);
    buf->size += 4;

#ifdef __APPLE__
    pthread_jit_write_protect_np(1);
#endif
}

void* dynasm_finalize(dynasm_buffer_t* buf) {
    if (!buf || buf->finalized) return buf ? buf->code : NULL;

#ifdef __APPLE__
    // Flush instruction cache on Apple Silicon
    sys_icache_invalidate(buf->code, buf->size);
#endif

    buf->finalized = 1;
    return buf->code;
}

// ============================================
// AArch64 instruction encoders
// ============================================

// MOVZ: Move wide with zero
// 64-bit: 1 10 100101 hw imm16 Rd
uint32_t aarch64_movz(int rd, uint16_t imm16, int shift) {
    uint32_t sf = 1;      // 64-bit
    uint32_t opc = 0b10;  // MOVZ
    uint32_t hw = shift & 0x3;

    return (sf << 31) | (opc << 29) | (0b100101 << 23) |
           (hw << 21) | ((uint32_t)imm16 << 5) | (rd & 0x1F);
}

// MOVK: Move wide with keep
// 64-bit: 1 11 100101 hw imm16 Rd
uint32_t aarch64_movk(int rd, uint16_t imm16, int shift) {
    uint32_t sf = 1;      // 64-bit
    uint32_t opc = 0b11;  // MOVK
    uint32_t hw = shift & 0x3;

    return (sf << 31) | (opc << 29) | (0b100101 << 23) |
           (hw << 21) | ((uint32_t)imm16 << 5) | (rd & 0x1F);
}

// ADD immediate
// 64-bit: 1 0 0 10001 sh imm12 Rn Rd
uint32_t aarch64_add_imm(int rd, int rn, uint16_t imm12) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b0010001 << 24) |
           ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// SUB immediate
// 64-bit: 1 1 0 10001 sh imm12 Rn Rd
uint32_t aarch64_sub_imm(int rd, int rn, uint16_t imm12) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b1010001 << 24) |
           ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// ADD register (shifted)
// 64-bit: 1 0 0 01011 sh 0 Rm imm6 Rn Rd
uint32_t aarch64_add_reg(int rd, int rn, int rm) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b0001011 << 24) |
           ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// SUB register (shifted)
// 64-bit: 1 1 0 01011 sh 0 Rm imm6 Rn Rd
uint32_t aarch64_sub_reg(int rd, int rn, int rm) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b1001011 << 24) |
           ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// MUL: Rd = Rn * Rm (alias of MADD with Ra=XZR)
// 64-bit: 1 00 11011 000 Rm 0 11111 Rn Rd
uint32_t aarch64_mul(int rd, int rn, int rm) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b0011011000 << 21) |
           ((rm & 0x1F) << 16) | (0b011111 << 10) |
           ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// SDIV: Rd = Rn / Rm (signed)
// 64-bit: 1 0 0 11010110 Rm 00001 1 Rn Rd
uint32_t aarch64_sdiv(int rd, int rn, int rm) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b0011010110 << 21) |
           ((rm & 0x1F) << 16) | (0b000011 << 10) |
           ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// RET: return (default x30)
uint32_t aarch64_ret(void) {
    return aarch64_ret_reg(30);
}

// RET with register
// 1101011 0 0 10 11111 0000 0 0 Rn 00000
uint32_t aarch64_ret_reg(int rn) {
    return (0b1101011 << 25) | (0b0010 << 21) | (0b11111 << 16) |
           ((rn & 0x1F) << 5);
}

// BLR: branch with link to register
// 1101011 0 0 01 11111 0000 0 0 Rn 00000
uint32_t aarch64_blr(int rn) {
    return (0b1101011 << 25) | (0b0001 << 21) | (0b11111 << 16) |
           ((rn & 0x1F) << 5);
}

// BR: branch to register
// 1101011 0 0 00 11111 0000 0 0 Rn 00000
uint32_t aarch64_br(int rn) {
    return (0b1101011 << 25) | (0b0000 << 21) | (0b11111 << 16) |
           ((rn & 0x1F) << 5);
}

// LDR (immediate, unsigned offset)
// 64-bit: 11 111 0 01 01 imm12 Rn Rt
uint32_t aarch64_ldr_imm(int rt, int rn, uint16_t offset) {
    return (0b11111001 << 24) | (0b01 << 22) |
           ((offset & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
}

// STR (immediate, unsigned offset)
// 64-bit: 11 111 0 01 00 imm12 Rn Rt
uint32_t aarch64_str_imm(int rt, int rn, uint16_t offset) {
    return (0b11111001 << 24) | (0b00 << 22) |
           ((offset & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (rt & 0x1F);
}

// STP (pre-index): Store pair with pre-index writeback
// 64-bit: 10 101 0 011 imm7 Rt2 Rn Rt
uint32_t aarch64_stp_pre(int rt1, int rt2, int rn, int16_t offset) {
    // offset is in units of 8 bytes, range -512 to 504
    int32_t imm7 = (offset / 8) & 0x7F;
    return (0b10101 << 27) | (0b0011 << 23) |
           (imm7 << 15) | ((rt2 & 0x1F) << 10) |
           ((rn & 0x1F) << 5) | (rt1 & 0x1F);
}

// LDP (post-index): Load pair with post-index writeback
// 64-bit: 10 101 0 001 imm7 Rt2 Rn Rt
uint32_t aarch64_ldp_post(int rt1, int rt2, int rn, int16_t offset) {
    // offset is in units of 8 bytes
    int32_t imm7 = (offset / 8) & 0x7F;
    return (0b10101 << 27) | (0b0001 << 23) |
           (imm7 << 15) | ((rt2 & 0x1F) << 10) |
           ((rn & 0x1F) << 5) | (rt1 & 0x1F);
}

// CMP register (alias of SUBS with Rd=XZR)
// 64-bit: 1 1 1 01011 sh 0 Rm imm6 Rn 11111
uint32_t aarch64_cmp_reg(int rn, int rm) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b1101011 << 24) |
           ((rm & 0x1F) << 16) | ((rn & 0x1F) << 5) | 0x1F;
}

// CMP immediate (alias of SUBS with Rd=XZR)
// 64-bit: 1 1 1 10001 sh imm12 Rn 11111
uint32_t aarch64_cmp_imm(int rn, uint16_t imm12) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b1110001 << 24) |
           ((imm12 & 0xFFF) << 10) | ((rn & 0x1F) << 5) | 0x1F;
}

// B.cond: conditional branch
// 0101010 0 imm19 0 cond
uint32_t aarch64_b_cond(int cond, int32_t offset) {
    uint32_t imm19 = offset & 0x7FFFF;
    return (0b0101010 << 25) | (imm19 << 5) | (cond & 0xF);
}

// B: unconditional branch
// 0 00101 imm26
uint32_t aarch64_b(int32_t offset) {
    uint32_t imm26 = offset & 0x3FFFFFF;
    return (0b000101 << 26) | imm26;
}

// NOP
uint32_t aarch64_nop(void) {
    return 0xD503201F;
}
