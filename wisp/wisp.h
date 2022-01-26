#ifndef WISP_H
#define WISP_H

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint32_t wisp_word_t;

typedef enum {
  WISP_BUILTIN_LAMBDA = 1,
  WISP_BUILTIN_MACRO,
  WISP_BUILTIN_CONS,
  WISP_BUILTIN_SET_SYMBOL_FUNCTION,
  WISP_BUILTIN_PLUS,
} wisp_builtin_t;

#define WISP_LOWTAG_BITS 3
#define WISP_WIDETAG_BITS 8

#define WISP_LOWTAG_MASK  ((1 << WISP_LOWTAG_BITS) - 1)
#define WISP_WIDETAG_MASK ((1 << WISP_WIDETAG_BITS) - 1)

#define WISP_LOWTAG(x)  ((x) & WISP_LOWTAG_MASK)
#define WISP_WIDETAG(x) ((x) & WISP_WIDETAG_MASK)

typedef enum wisp_lowtag {
  /* 0 */ WISP_LOWTAG_FIXNUM_0,
  /* 1 */ WISP_LOWTAG_FUNCTION_PTR,
  /* 2 */ WISP_LOWTAG_OTHER_IMMEDIATE_0,
  /* 3 */ WISP_LOWTAG_LIST_PTR,
  /* 4 */ WISP_LOWTAG_FIXNUM_1,
  /* 5 */ WISP_LOWTAG_STRUCT_PTR,
  /* 6 */ WISP_LOWTAG_OTHER_IMMEDIATE_1,
  /* 7 */ WISP_LOWTAG_OTHER_PTR,
} wisp_lowtag_t;

typedef enum wisp_widetag {
  WISP_WIDETAG_INSTANCE = 0xC2,
  WISP_WIDETAG_STRING = 0x32,
  WISP_WIDETAG_SYMBOL = 0xAE,
  WISP_WIDETAG_BUILTIN = 0xA2,
} wisp_widetag_t;

#define WISP_IS_FIXNUM(x) (((x) & 3) == 0)
#define WISP_IS_OTHER_IMMEDIATE(x) (((x) & 3) == 2)
#define WISP_IS_PTR(x) ((x) & 1)

#define WISP_IS_OTHER_PTR(x) \
  (WISP_LOWTAG(x) == WISP_LOWTAG_OTHER_PTR)
#define WISP_IS_LIST_PTR(x) \
  (WISP_LOWTAG(x) == WISP_LOWTAG_LIST_PTR)
#define WISP_IS_STRUCT_PTR(x) \
  (WISP_LOWTAG(x) == WISP_LOWTAG_STRUCT_PTR)

#define WISP_ALIGNED_PROPERLY(x) \
  (((x) & WISP_LOWTAG_MASK) == 0)

extern void *heap;

static const wisp_word_t NIL =
  WISP_LOWTAG_LIST_PTR;

extern wisp_word_t APPLY;
extern wisp_word_t CLOSURE;
extern wisp_word_t COMMON_LISP;
extern wisp_word_t EVAL;
extern wisp_word_t LAMBDA;
extern wisp_word_t MACRO;
extern wisp_word_t PACKAGE;
extern wisp_word_t PARAMS;
extern wisp_word_t QUOTE;
extern wisp_word_t SCOPE;
extern wisp_word_t SET_SYMBOL_FUNCTION;

wisp_word_t
wisp_read (const char **s);

wisp_word_t
wisp_cons (wisp_word_t car, wisp_word_t cdr);

wisp_word_t
wisp_string_n (const char *source, int length);

char *
wisp_string_buffer (wisp_word_t *header);

wisp_word_t *
wisp_deref (wisp_word_t ptr);

wisp_word_t
wisp_intern_symbol (wisp_word_t name,
                    wisp_word_t package);

__attribute__ ((noreturn))
void
wisp_not_implemented ();

wisp_word_t
wisp_fixnum (int32_t x);

#endif
