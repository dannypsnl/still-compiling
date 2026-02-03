#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

/* Low 3-bit tag scheme on int64_t */
#define TAG_MASK   0x7
#define TAG_INT    0  /* 000 */
#define TAG_PAIR   1  /* 001 */
#define TAG_VEC    2  /* 010 */
#define TAG_CLOS   3  /* 011 */
#define TAG_FLOAT  4  /* 100 */
#define TAG_BOOL   5  /* 101 */
#define TAG_IMM    6  /* 110 - null and void distinguished by payload */
#define TAG_STRING 7  /* 111 */

#define MAKE_INT(n)   ((int64_t)(n) << 3)
#define GET_INT(v)    ((int64_t)(v) >> 3)
#define UNTAG_PTR(v)  ((void *)((v) & ~(int64_t)TAG_MASK))
#define GET_TAG(v)    ((v) & TAG_MASK)

#define SCM_TRUE   13  /* (1 << 3) | TAG_BOOL */
#define SCM_FALSE  5   /* (0 << 3) | TAG_BOOL */
#define SCM_NULL   6   /* (0 << 3) | TAG_IMM */
#define SCM_VOID   14  /* (1 << 3) | TAG_IMM */

#define IS_INT(v)   (GET_TAG(v) == TAG_INT)
#define IS_FLOAT(v) (GET_TAG(v) == TAG_FLOAT)
#define IS_NUM(v)   (IS_INT(v) || IS_FLOAT(v))

/* ----- Pair (cons) ----- */
int64_t scm_cons(int64_t car, int64_t cdr) {
    int64_t *pair = (int64_t *)malloc(2 * sizeof(int64_t));
    pair[0] = car;
    pair[1] = cdr;
    return (int64_t)pair | TAG_PAIR;
}
int64_t scm_car(int64_t p) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(p);
    return ptr[0];
}
int64_t scm_cdr(int64_t p) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(p);
    return ptr[1];
}

/* ----- Closure ----- */
int64_t scm_make_closure(int64_t code, int64_t env) {
    int64_t *clos = (int64_t *)malloc(2 * sizeof(int64_t));
    clos[0] = code;
    clos[1] = env;
    return (int64_t)clos | TAG_CLOS;
}
int64_t scm_closure_code(int64_t c) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(c);
    return ptr[0];
}
int64_t scm_closure_env(int64_t c) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(c);
    return ptr[1];
}

/* ----- Vector ----- */
int64_t scm_make_vector(int64_t n) {
    /* n is a tagged integer; slot 0 = tagged length, slots 1..n = elements */
    int64_t raw_n = GET_INT(n);
    int64_t *vec = (int64_t *)malloc((raw_n + 1) * sizeof(int64_t));
    vec[0] = n; /* store tagged length */
    return (int64_t)vec | TAG_VEC;
}
int64_t scm_vector_ref(int64_t vec, int64_t idx) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(vec);
    return ptr[GET_INT(idx) + 1]; /* skip slot 0 (length) */
}
int64_t scm_vector_set(int64_t vec, int64_t idx, int64_t val) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(vec);
    ptr[GET_INT(idx) + 1] = val;
    return SCM_NULL;
}
int64_t scm_vector_length(int64_t vec) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(vec);
    return ptr[0]; /* already tagged */
}

/* ----- Float (boxed double) ----- */
int64_t scm_make_float(double d) {
    double *box = (double *)malloc(sizeof(double));
    *box = d;
    return (int64_t)box | TAG_FLOAT;
}
double scm_get_float(int64_t v) {
    double *box = (double *)UNTAG_PTR(v);
    return *box;
}

/* Convert tagged value to double (works for both int and float) */
static double to_double(int64_t v) {
    if (IS_INT(v)) return (double)GET_INT(v);
    if (IS_FLOAT(v)) return scm_get_float(v);
    fprintf(stderr, "to_double: not a number\n");
    exit(1);
}

/* ----- Arithmetic (auto-promote) ----- */
int64_t scm_add(int64_t a, int64_t b) {
    if (IS_INT(a) && IS_INT(b))
        return MAKE_INT(GET_INT(a) + GET_INT(b));
    return scm_make_float(to_double(a) + to_double(b));
}
int64_t scm_sub(int64_t a, int64_t b) {
    if (IS_INT(a) && IS_INT(b))
        return MAKE_INT(GET_INT(a) - GET_INT(b));
    return scm_make_float(to_double(a) - to_double(b));
}
int64_t scm_mul(int64_t a, int64_t b) {
    if (IS_INT(a) && IS_INT(b))
        return MAKE_INT(GET_INT(a) * GET_INT(b));
    return scm_make_float(to_double(a) * to_double(b));
}
int64_t scm_div(int64_t a, int64_t b) {
    /* Always produce float for division if not exact */
    if (IS_INT(a) && IS_INT(b) && GET_INT(b) != 0 && GET_INT(a) % GET_INT(b) == 0)
        return MAKE_INT(GET_INT(a) / GET_INT(b));
    return scm_make_float(to_double(a) / to_double(b));
}

/* ----- Comparisons → tagged bool ----- */
int64_t scm_eq(int64_t a, int64_t b) {
    if (IS_INT(a) && IS_INT(b)) return (a == b) ? SCM_TRUE : SCM_FALSE;
    return (to_double(a) == to_double(b)) ? SCM_TRUE : SCM_FALSE;
}
int64_t scm_lt(int64_t a, int64_t b) {
    if (IS_INT(a) && IS_INT(b)) return (GET_INT(a) < GET_INT(b)) ? SCM_TRUE : SCM_FALSE;
    return (to_double(a) < to_double(b)) ? SCM_TRUE : SCM_FALSE;
}
int64_t scm_gt(int64_t a, int64_t b) {
    if (IS_INT(a) && IS_INT(b)) return (GET_INT(a) > GET_INT(b)) ? SCM_TRUE : SCM_FALSE;
    return (to_double(a) > to_double(b)) ? SCM_TRUE : SCM_FALSE;
}
int64_t scm_le(int64_t a, int64_t b) {
    if (IS_INT(a) && IS_INT(b)) return (GET_INT(a) <= GET_INT(b)) ? SCM_TRUE : SCM_FALSE;
    return (to_double(a) <= to_double(b)) ? SCM_TRUE : SCM_FALSE;
}
int64_t scm_ge(int64_t a, int64_t b) {
    if (IS_INT(a) && IS_INT(b)) return (GET_INT(a) >= GET_INT(b)) ? SCM_TRUE : SCM_FALSE;
    return (to_double(a) >= to_double(b)) ? SCM_TRUE : SCM_FALSE;
}

/* ----- Predicates → tagged bool ----- */
int64_t scm_is_null(int64_t v) { return (v == SCM_NULL) ? SCM_TRUE : SCM_FALSE; }
int64_t scm_is_pair(int64_t v) { return (GET_TAG(v) == TAG_PAIR) ? SCM_TRUE : SCM_FALSE; }
int64_t scm_is_number(int64_t v) { return IS_NUM(v) ? SCM_TRUE : SCM_FALSE; }
int64_t scm_is_boolean(int64_t v) { return (GET_TAG(v) == TAG_BOOL) ? SCM_TRUE : SCM_FALSE; }
int64_t scm_is_vector(int64_t v) { return (GET_TAG(v) == TAG_VEC) ? SCM_TRUE : SCM_FALSE; }
int64_t scm_is_procedure(int64_t v) { return (GET_TAG(v) == TAG_CLOS) ? SCM_TRUE : SCM_FALSE; }

/* ----- Logic ----- */
int64_t scm_not(int64_t v) { return (v == SCM_FALSE) ? SCM_TRUE : SCM_FALSE; }

/* ----- String ----- */
/* Layout: [tagged_length (i64)] [char bytes... (null-terminated)] */
int64_t scm_make_string(int64_t len) {
    int64_t raw_len = GET_INT(len);
    int64_t *str = (int64_t *)malloc(sizeof(int64_t) + raw_len + 1);
    str[0] = len; /* store tagged length */
    char *chars = (char *)(str + 1);
    memset(chars, 0, raw_len + 1);
    return (int64_t)str | TAG_STRING;
}
int64_t scm_string_from_cstr(int64_t cstr_ptr, int64_t len) {
    const char *s = (const char *)cstr_ptr;
    int64_t raw_len = GET_INT(len);
    int64_t *str = (int64_t *)malloc(sizeof(int64_t) + raw_len + 1);
    str[0] = len; /* store tagged length */
    char *chars = (char *)(str + 1);
    memcpy(chars, s, raw_len);
    chars[raw_len] = '\0';
    return (int64_t)str | TAG_STRING;
}
int64_t scm_string_ref(int64_t str, int64_t idx) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(str);
    char *chars = (char *)(ptr + 1);
    return MAKE_INT((unsigned char)chars[GET_INT(idx)]);
}
int64_t scm_string_set(int64_t str, int64_t idx, int64_t ch) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(str);
    char *chars = (char *)(ptr + 1);
    chars[GET_INT(idx)] = (char)GET_INT(ch);
    return SCM_VOID;
}
int64_t scm_string_length(int64_t str) {
    int64_t *ptr = (int64_t *)UNTAG_PTR(str);
    return ptr[0]; /* already tagged */
}
int64_t scm_string_append(int64_t a, int64_t b) {
    int64_t *pa = (int64_t *)UNTAG_PTR(a);
    int64_t *pb = (int64_t *)UNTAG_PTR(b);
    int64_t len_a = GET_INT(pa[0]);
    int64_t len_b = GET_INT(pb[0]);
    int64_t total = len_a + len_b;
    int64_t *str = (int64_t *)malloc(sizeof(int64_t) + total + 1);
    str[0] = MAKE_INT(total);
    char *chars = (char *)(str + 1);
    memcpy(chars, (char *)(pa + 1), len_a);
    memcpy(chars + len_a, (char *)(pb + 1), len_b);
    chars[total] = '\0';
    return (int64_t)str | TAG_STRING;
}
int64_t scm_is_string(int64_t v) {
    return (GET_TAG(v) == TAG_STRING) ? SCM_TRUE : SCM_FALSE;
}

/* ----- Display ----- */
static void display_val(int64_t v) {
    switch (GET_TAG(v)) {
    case TAG_INT:
        printf("%" PRId64, GET_INT(v));
        break;
    case TAG_FLOAT:
        printf("%g", scm_get_float(v));
        break;
    case TAG_BOOL:
        printf("%s", (v == SCM_TRUE) ? "#t" : "#f");
        break;
    case TAG_IMM:
        if (v == SCM_NULL)
            printf("()");
        /* void prints nothing */
        break;
    case TAG_PAIR: {
        printf("(");
        int64_t cur = v;
        int first = 1;
        while (GET_TAG(cur) == TAG_PAIR) {
            if (!first) printf(" ");
            first = 0;
            display_val(scm_car(cur));
            cur = scm_cdr(cur);
        }
        if (cur != SCM_NULL) {
            printf(" . ");
            display_val(cur);
        }
        printf(")");
        break;
    }
    case TAG_VEC: {
        int64_t *ptr = (int64_t *)UNTAG_PTR(v);
        int64_t len = GET_INT(ptr[0]);
        printf("#(");
        for (int64_t i = 0; i < len; i++) {
            if (i > 0) printf(" ");
            display_val(ptr[i + 1]);
        }
        printf(")");
        break;
    }
    case TAG_CLOS:
        printf("#<closure>");
        break;
    case TAG_STRING: {
        int64_t *ptr = (int64_t *)UNTAG_PTR(v);
        char *chars = (char *)(ptr + 1);
        printf("%s", chars);
        break;
    }
    default:
        fprintf(stderr, "#<unknown:%" PRId64 ">", v);
        exit(1);
        break;
    }
}

int64_t scm_display(int64_t v) {
    display_val(v);
    return SCM_VOID;
}
int64_t scm_displayln(int64_t v) {
    display_val(v);
    printf("\n");
    return SCM_VOID;
}
