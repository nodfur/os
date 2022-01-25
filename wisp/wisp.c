#define WISP_DEBUG_ALLOC 0

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uint32_t wisp_word_t;

#define WISP_TAG_MASK 0x7
#define WISP_TAG(x) ((x) & WISP_LOWTAG_MASK)

#define WISP_IS_FIXNUM(x) \
  ((x) & 0x3 == 0)

#define WISP_IS_OTHER_IMMEDIATE(x) \
  ((x) & 0x3 == 1)

#define WISP_IS_PTR(x) \
  ((x) & 1)

void *heap;
size_t heap_size;
size_t heap_used;

wisp_word_t
wisp_fixnum (int32_t x)
{
  return x << 2;
}
