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

// MSUB: Rd = Ra - Rn * Rm
// 64-bit: 1 00 11011 000 Rm 1 Ra Rn Rd
uint32_t aarch64_msub(int rd, int rn, int rm, int ra) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b0011011000 << 21) |
           ((rm & 0x1F) << 16) | (1 << 15) | ((ra & 0x1F) << 10) |
           ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// LSR immediate (alias of UBFM)
// 64-bit: 1 10 100110 1 immr imms Rn Rd
// LSR Rd, Rn, #shift => UBFM Rd, Rn, #shift, #63
uint32_t aarch64_lsr_imm(int rd, int rn, int imm6) {
    uint32_t sf = 1;  // 64-bit
    uint32_t N = 1;   // 64-bit
    uint32_t immr = imm6 & 0x3F;
    uint32_t imms = 63;  // for 64-bit LSR
    return (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (N << 22) |
           (immr << 16) | (imms << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// LSR register
// 64-bit: 1 0 0 11010110 Rm 0010 01 Rn Rd
uint32_t aarch64_lsr_reg(int rd, int rn, int rm) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b0011010110 << 21) |
           ((rm & 0x1F) << 16) | (0b001001 << 10) |
           ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// LSL immediate (alias of UBFM)
// 64-bit: LSL Rd, Rn, #shift => UBFM Rd, Rn, #(-shift mod 64), #(63-shift)
uint32_t aarch64_lsl_imm(int rd, int rn, int imm6) {
    uint32_t sf = 1;  // 64-bit
    uint32_t N = 1;   // 64-bit
    uint32_t immr = (-imm6) & 0x3F;
    uint32_t imms = (63 - imm6) & 0x3F;
    return (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (N << 22) |
           (immr << 16) | (imms << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// LSL register
// 64-bit: 1 0 0 11010110 Rm 0010 00 Rn Rd
uint32_t aarch64_lsl_reg(int rd, int rn, int rm) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b0011010110 << 21) |
           ((rm & 0x1F) << 16) | (0b001000 << 10) |
           ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// AND immediate - simplified for power-of-2-minus-1 masks (like 0x1)
// For imm=1: N=1, immr=0, imms=0 gives mask 0x1
// 64-bit: 1 00 100100 N immr imms Rn Rd
uint32_t aarch64_and_imm(int rd, int rn, uint64_t imm) {
    uint32_t sf = 1;  // 64-bit
    uint32_t N = 1;
    uint32_t immr = 0;
    uint32_t imms = 0;

    // Handle simple case: imm = 1 (test lowest bit)
    if (imm == 1) {
        N = 1; immr = 0; imms = 0;
    }
    // Add more cases as needed

    return (sf << 31) | (0b00 << 29) | (0b100100 << 23) | (N << 22) |
           (immr << 16) | (imms << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// TST immediate (alias of ANDS with Rd=XZR)
// 64-bit: 1 11 100100 N immr imms Rn 11111
uint32_t aarch64_tst_imm(int rn, uint64_t imm) {
    uint32_t sf = 1;  // 64-bit
    uint32_t N = 1;
    uint32_t immr = 0;
    uint32_t imms = 0;

    // Handle simple case: imm = 1 (test lowest bit)
    if (imm == 1) {
        N = 1; immr = 0; imms = 0;
    }

    return (sf << 31) | (0b11 << 29) | (0b100100 << 23) | (N << 22) |
           (immr << 16) | (imms << 10) | ((rn & 0x1F) << 5) | 0x1F;
}

// CLZ: Count Leading Zeros
// 64-bit: 1 1 0 11010110 00000 00010 0 Rn Rd
uint32_t aarch64_clz(int rd, int rn) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b1011010110 << 21) | (0b00000 << 16) |
           (0b000100 << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// RBIT: Reverse Bits
// 64-bit: 1 1 0 11010110 00000 00000 0 Rn Rd
uint32_t aarch64_rbit(int rd, int rn) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b1011010110 << 21) | (0b00000 << 16) |
           (0b000000 << 10) | ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// CSEL: Conditional Select
// 64-bit: 1 0 0 11010100 Rm cond 0 0 Rn Rd
uint32_t aarch64_csel(int rd, int rn, int rm, int cond) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b0011010100 << 21) |
           ((rm & 0x1F) << 16) | ((cond & 0xF) << 12) |
           ((rn & 0x1F) << 5) | (rd & 0x1F);
}

// CSNEG: Conditional Select Negation
// 64-bit: 1 1 0 11010100 Rm cond 0 1 Rn Rd
uint32_t aarch64_csneg(int rd, int rn, int rm, int cond) {
    uint32_t sf = 1;  // 64-bit
    return (sf << 31) | (0b1011010100 << 21) |
           ((rm & 0x1F) << 16) | ((cond & 0xF) << 12) | (1 << 10) |
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

// ============================================
// SIMD/NEON instruction encoders
// ============================================

// Helper: get Q bit and size bits from arrangement
static void arr_to_qsize(int arr, uint32_t* q, uint32_t* size) {
    switch (arr) {
        case SIMD_8B:  *q = 0; *size = 0; break;
        case SIMD_16B: *q = 1; *size = 0; break;
        case SIMD_4H:  *q = 0; *size = 1; break;
        case SIMD_8H:  *q = 1; *size = 1; break;
        case SIMD_2S:  *q = 0; *size = 2; break;
        case SIMD_4S:  *q = 1; *size = 2; break;
        case SIMD_2D:  *q = 1; *size = 3; break;
        default:       *q = 1; *size = 2; break; // default to 4S
    }
}

// LDR (SIMD, immediate unsigned offset)
// Q=1 for 128-bit: 00 111101 11 imm12 Rn Rt
uint32_t aarch64_ldr_simd(int vt, int rn, uint16_t offset) {
    // 128-bit (Q) variant: opc=11, size=00
    return (0b00111101 << 24) | (0b11 << 22) |
           ((offset & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (vt & 0x1F);
}

// STR (SIMD, immediate unsigned offset)
// Q=1 for 128-bit: 00 111101 10 imm12 Rn Rt
uint32_t aarch64_str_simd(int vt, int rn, uint16_t offset) {
    return (0b00111101 << 24) | (0b10 << 22) |
           ((offset & 0xFFF) << 10) | ((rn & 0x1F) << 5) | (vt & 0x1F);
}

// DUP (element): duplicate scalar element to all lanes
// 0 Q 0 01110 000 imm5 0 00001 Rn Rd
uint32_t aarch64_dup_element(int vd, int vn, int arr, int index) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    // imm5 encoding: (index << (size+1)) | (1 << size)
    // For D (size=3): bits 3:0 = 1000, index in bit 4
    uint32_t imm5 = (index << (size + 1)) | (1 << size);
    return (q << 30) | (0b001110000 << 21) | ((imm5 & 0x1F) << 16) |
           (0b000001 << 10) | ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// DUP (general): duplicate GP register to vector
// 0 Q 0 01110 000 imm5 0 00011 Rn Rd
uint32_t aarch64_dup_general(int vd, int rn, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    uint32_t imm5 = 1 << size;
    return (q << 30) | (0b001110000 << 21) | ((imm5 & 0x1F) << 16) |
           (0b000011 << 10) | ((rn & 0x1F) << 5) | (vd & 0x1F);
}

// ADD (vector): 0 Q 0 01110 sz 1 Rm 10000 1 Rn Rd
uint32_t aarch64_add_simd(int vd, int vn, int vm, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b001110 << 24) | (size << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b100001 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// SUB (vector): 0 Q 1 01110 sz 1 Rm 10000 1 Rn Rd
uint32_t aarch64_sub_simd(int vd, int vn, int vm, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b101110 << 24) | (size << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b100001 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// MUL (vector): 0 Q 0 01110 sz 1 Rm 10011 1 Rn Rd
// Note: not valid for size=3 (2D)
uint32_t aarch64_mul_simd(int vd, int vn, int vm, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b001110 << 24) | (size << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b100111 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FADD (vector): 0 Q 0 01110 0 sz 1 Rm 11010 1 Rn Rd
uint32_t aarch64_fadd_simd(int vd, int vn, int vm, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;  // 0=float32, 1=float64
    return (q << 30) | (0b001110 << 24) | (sz << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b110101 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FSUB (vector): 0 Q 0 01110 1 sz 1 Rm 11010 1 Rn Rd
uint32_t aarch64_fsub_simd(int vd, int vn, int vm, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b001110 << 24) | (1 << 23) | (sz << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b110101 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FMUL (vector): 0 Q 1 01110 0 sz 1 Rm 11011 1 Rn Rd
uint32_t aarch64_fmul_simd(int vd, int vn, int vm, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b101110 << 24) | (sz << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b110111 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FDIV (vector): 0 Q 1 01110 0 sz 1 Rm 11111 1 Rn Rd
uint32_t aarch64_fdiv_simd(int vd, int vn, int vm, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b101110 << 24) | (sz << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b111111 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// ADDV (across vector): 0 Q 0 01110 sz 11000 1 1011 10 Rn Rd
uint32_t aarch64_addv(int vd, int vn, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b001110 << 24) | (size << 22) | (0b110001 << 16) |
           (0b101110 << 10) | ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FADDP (vector): 0 Q 1 01110 0 sz 1 Rm 11010 1 Rn Rd
uint32_t aarch64_faddp_simd(int vd, int vn, int vm, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b101110 << 24) | (sz << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b110101 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FMLA (vector): 0 Q 0 01110 0 sz 1 Rm 11001 1 Rn Rd
uint32_t aarch64_fmla_simd(int vd, int vn, int vm, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b001110 << 24) | (sz << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b110011 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// MLA (vector): 0 Q 0 01110 sz 1 Rm 10010 1 Rn Rd
uint32_t aarch64_mla_simd(int vd, int vn, int vm, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b001110 << 24) | (size << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b100101 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// MOVI: 0 Q op 0111100000 a b c cmode 0 1 d e f g h Rd
// Simple version: 32-bit lanes with shift
uint32_t aarch64_movi(int vd, uint8_t imm8, int arr) {
    uint32_t q = (arr == SIMD_16B || arr == SIMD_8H || arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    // For 8B/16B with imm8 replicated to all bytes
    // cmode = 1110, op = 0
    uint32_t a = (imm8 >> 7) & 1;
    uint32_t b = (imm8 >> 6) & 1;
    uint32_t c = (imm8 >> 5) & 1;
    uint32_t d = (imm8 >> 4) & 1;
    uint32_t e = (imm8 >> 3) & 1;
    uint32_t f = (imm8 >> 2) & 1;
    uint32_t g = (imm8 >> 1) & 1;
    uint32_t h = imm8 & 1;
    return (q << 30) | (0b0111100000 << 20) | (a << 18) | (b << 17) | (c << 16) |
           (0b1110 << 12) | (0b01 << 10) | (d << 9) | (e << 8) | (f << 7) |
           (g << 6) | (h << 5) | (vd & 0x1F);
}

// SCVTF (vector, integer): 0 Q 0 01110 0 sz 1 00001 11010 Rn Rd
uint32_t aarch64_scvtf_simd(int vd, int vn, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b001110 << 24) | (sz << 22) | (0b100001 << 16) |
           (0b110110 << 10) | ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FCVTZS (vector, integer): 0 Q 0 01110 1 sz 1 00001 10111 Rn Rd
uint32_t aarch64_fcvtzs_simd(int vd, int vn, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b001110 << 24) | (1 << 23) | (sz << 22) | (0b100001 << 16) |
           (0b101110 << 10) | ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// SMAX (vector): 0 Q 0 01110 sz 1 Rm 01100 1 Rn Rd
uint32_t aarch64_smax_simd(int vd, int vn, int vm, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b001110 << 24) | (size << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b011001 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// SMIN (vector): 0 Q 0 01110 sz 1 Rm 01101 1 Rn Rd
uint32_t aarch64_smin_simd(int vd, int vn, int vm, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b001110 << 24) | (size << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b011011 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FMAX (vector): 0 Q 0 01110 0 sz 1 Rm 11110 1 Rn Rd
uint32_t aarch64_fmax_simd(int vd, int vn, int vm, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b001110 << 24) | (sz << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b111101 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FMIN (vector): 0 Q 0 01110 1 sz 1 Rm 11110 1 Rn Rd
uint32_t aarch64_fmin_simd(int vd, int vn, int vm, int arr) {
    uint32_t q = (arr == SIMD_4S || arr == SIMD_2D) ? 1 : 0;
    uint32_t sz = (arr == SIMD_2D) ? 1 : 0;
    return (q << 30) | (0b001110 << 24) | (1 << 23) | (sz << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b111101 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// SMAXV (across vector): 0 Q 0 01110 sz 11000 0 1010 10 Rn Rd
uint32_t aarch64_smaxv(int vd, int vn, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b001110 << 24) | (size << 22) | (0b110000 << 16) |
           (0b101010 << 10) | ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// SMINV (across vector): 0 Q 0 01110 sz 11000 1 1010 10 Rn Rd
uint32_t aarch64_sminv(int vd, int vn, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    return (q << 30) | (0b001110 << 24) | (size << 22) | (0b110001 << 16) |
           (0b101010 << 10) | ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FMAXV (across vector, 4S only): 0 1 1 01110 00 11000 0 1111 10 Rn Rd
uint32_t aarch64_fmaxv(int vd, int vn) {
    return (0b01101110 << 24) | (0b00110000 << 16) |
           (0b111110 << 10) | ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FMINV (across vector, 4S only): 0 1 1 01110 10 11000 0 1111 10 Rn Rd
uint32_t aarch64_fminv(int vd, int vn) {
    return (0b01101110 << 24) | (1 << 23) | (0b0110000 << 16) |
           (0b111110 << 10) | ((vn & 0x1F) << 5) | (vd & 0x1F);
}

// FMOV (general to vector, 64-bit): 1 00 11110 01 1 00 111 000000 Rn Rd
uint32_t aarch64_fmov_gp_to_vec(int vd, int rn) {
    return (0b100111100 << 23) | (0b11 << 21) | (0b00111 << 16) |
           ((rn & 0x1F) << 5) | (vd & 0x1F);
}

// FMOV (vector to general, 64-bit): 1 00 11110 01 1 00 110 000000 Rn Rd
uint32_t aarch64_fmov_vec_to_gp(int rd, int vn) {
    return (0b100111100 << 23) | (0b11 << 21) | (0b00110 << 16) |
           ((vn & 0x1F) << 5) | (rd & 0x1F);
}

// UMOV: 0 Q 0 01110 000 imm5 0 01111 Rn Rd
uint32_t aarch64_umov(int rd, int vn, int arr, int index) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    // For UMOV, Q must be 1 for 64-bit result (2D), 0 for 32-bit (4S or smaller)
    // imm5 encoding: index << (size+1) | (1 << size)
    uint32_t imm5 = (index << (size + 1)) | (1 << size);
    // For 64-bit transfer (D element), Q=1
    uint32_t q_bit = (arr == SIMD_2D) ? 1 : 0;
    return (q_bit << 30) | (0b001110000 << 21) | ((imm5 & 0x1F) << 16) |
           (0b001111 << 10) | ((vn & 0x1F) << 5) | (rd & 0x1F);
}

// INS (general): 0 1 0 01110 000 imm5 0 0011 1 Rn Rd
uint32_t aarch64_ins_general(int vd, int rn, int arr, int index) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    uint32_t imm5 = (index << (size + 1)) | (1 << size);
    return (0b01001110000 << 21) | ((imm5 & 0x1F) << 16) |
           (0b000111 << 10) | ((rn & 0x1F) << 5) | (vd & 0x1F);
}

// EOR (vector): 0 Q 10 1110 sz 1 Rm 0001 11 Rn Rd
uint32_t aarch64_eor_simd(int vd, int vn, int vm, int arr) {
    uint32_t q, size;
    arr_to_qsize(arr, &q, &size);
    // EOR always uses size=00 in encoding (operates on bytes regardless of arr)
    return (q << 30) | (0b101110 << 24) | (0b00 << 22) | (1 << 21) |
           ((vm & 0x1F) << 16) | (0b000111 << 10) |
           ((vn & 0x1F) << 5) | (vd & 0x1F);
}
