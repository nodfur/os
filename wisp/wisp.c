//
// This is a Lisp interpreter that compiles to WebAssembly.
//
// Wisp supports first-class delimited continuations.
//
// The Wisp execution state is serializable like in Smalltalk.
//

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef int32_t wisp_fixnum_t;

typedef struct wisp_value_t wisp_value_t;

typedef struct {
  size_t heap_size;

  void *live_heap;
  void *live_heap_tail;
  void *idle_heap;

  wisp_value_t *nil;
} wisp_t;

typedef enum {
  WISP_TAG_NIL,
  WISP_TAG_CONS,
  WISP_TAG_VECTOR,
  WISP_TAG_FIXNUM,
  WISP_TAG_SYMBOL
} wisp_tag_t;

typedef struct {
  wisp_value_t *car;
  wisp_value_t *cdr;
} wisp_cons_t;

typedef struct {
  wisp_fixnum_t size;
  wisp_value_t *first;
} wisp_vector_t;

struct wisp_value_t {
  wisp_tag_t tag;

  union {
    wisp_fixnum_t fixnum;
    wisp_value_t *car;
  };

  wisp_value_t *cdr;
};

const size_t WISP_VALUE_SIZE_1 =
  sizeof (wisp_tag_t) + sizeof (wisp_fixnum_t);

const size_t WISP_VALUE_SIZE_2 =
  sizeof (wisp_tag_t) + sizeof (wisp_cons_t);

size_t
wisp_free_space (wisp_t *ctx)
{
  size_t live_bytes_used =
    ctx->live_heap_tail - ctx->live_heap;

  size_t live_bytes_left =
    ctx->heap_size - live_bytes_used;

  return live_bytes_left;
}

void
wisp_tidy (wisp_t *ctx)
{
  // noop
}

typedef enum {
  WISP_ERROR_HEAP_FULL
} wisp_error_t;

const char *wisp_error_names[] = {
  "WISP-ERROR-HEAP-FULL"
};

wisp_t *
wisp_crash (wisp_t *ctx, wisp_error_t error)
{
  fprintf (stderr, "wisp crash: %s\n", wisp_error_names[error]);
  exit (1);
  return ctx;
}

void *
wisp_alloc (wisp_t *ctx, size_t n)
{
  if (wisp_free_space(ctx) < n)
    wisp_tidy (ctx);

  if (wisp_free_space(ctx) < n)
    return wisp_crash (ctx, WISP_ERROR_HEAP_FULL);

  void *pointer = ctx->live_heap_tail;

  ctx->live_heap_tail += n;

  return pointer;
}

wisp_value_t *
wisp_cons (wisp_t *ctx,
           wisp_value_t *car,
           wisp_value_t *cdr)
{
  wisp_value_t *value =
    wisp_alloc (ctx, WISP_VALUE_SIZE_2);

  value->tag = WISP_TAG_CONS;
  value->car = car;
  value->cdr = cdr;

  return value;
}

wisp_value_t *
wisp_alloc_vector (wisp_t *ctx,
                   wisp_fixnum_t entries) {
  size_t size =
    entries * sizeof (wisp_value_t *);

  wisp_value_t *value =
    wisp_alloc (ctx, WISP_VALUE_SIZE_2 + size);

  value->tag = WISP_TAG_VECTOR;
  value->fixnum = entries;

  return value;
}

wisp_value_t *
wisp_vector (wisp_t *ctx,
             wisp_fixnum_t entries,
             ...)
{
  va_list args;
  va_start (args, entries);

  wisp_value_t *value =
    wisp_alloc_vector (ctx, entries);

  for (int i = 0; i < entries; i++)
    (&value->cdr)[i] = va_arg (args, wisp_value_t *);

  va_end (args);

  return value;
}

wisp_value_t *
wisp_alloc_1 (wisp_t *ctx, wisp_tag_t tag)
{
  wisp_value_t *value = wisp_alloc (ctx, WISP_VALUE_SIZE_1);

  value->tag = tag;

  return value;
}

wisp_value_t *
wisp_fixnum (wisp_t *ctx,
             wisp_fixnum_t fixnum)
{
  wisp_value_t *value = wisp_alloc_1 (ctx, WISP_TAG_FIXNUM);

  value->fixnum = fixnum;

  return value;
}

wisp_value_t *
wisp_nil (wisp_t *ctx)
{
  return ctx->nil;
}

wisp_t
wisp_start (size_t heap_size)
{
  uint8_t *live_heap = malloc (heap_size);
  uint8_t *idle_heap = malloc (heap_size);

  size_t heap_megabytes = heap_size / (1024 * 1024);

  fprintf (stderr, "wisp: live heap %p (%lu MB)\n",
           live_heap, heap_megabytes);

  fprintf (stderr, "wisp: idle heap %p (%lu MB)\n",
           idle_heap, heap_megabytes);

  wisp_t ctx = (wisp_t) {
    .heap_size = heap_size,
    .live_heap = live_heap,
    .live_heap_tail = live_heap,
    .idle_heap = idle_heap,
    .nil = NULL
  };

  ctx.nil = wisp_alloc_1 (&ctx, WISP_TAG_NIL);

  return ctx;
}

void
wisp_stop (wisp_t *ctx)
{
  free (ctx->live_heap);
  free (ctx->idle_heap);
  fprintf (stderr, "wisp: stop\n");
}

wisp_value_t *
wisp_cps (wisp_t *ctx,
          wisp_value_t *e)
{
  switch (e->tag)
    {
    case WISP_TAG_NIL:
    case WISP_TAG_FIXNUM:
      return e;

    case WISP_TAG_CONS:
      // (f 0 (x)) => (x (\xv. (f 0 xv)))

    }
}



int
main (int argc, char **argv)
{
  wisp_t ctx = wisp_start (4 * 1024 * 1024);

  wisp_stop (&ctx);
}
