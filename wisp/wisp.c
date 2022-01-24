//
// This is a Lisp interpreter that compiles to WebAssembly.
//
// Wisp supports first-class delimited continuations.
//
// The Wisp execution state is serializable like in Smalltalk.
//

#define WISP_DEBUG_ALLOC 0

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int32_t wisp_fixnum_t;

typedef struct wisp_value wisp_value_t;

typedef wisp_fixnum_t wisp_ptr_t;

typedef struct wisp_basics {
  wisp_ptr_t NIL;
  wisp_ptr_t PACKAGE;
  wisp_ptr_t QUOTE;
  wisp_ptr_t T;

  wisp_ptr_t DONE;
} wisp_basics_t;

typedef struct wisp {
  size_t heap_size;

  void *live_heap;
  void *live_heap_tail;
  void *idle_heap;

  struct {
    wisp_ptr_t LISP;
  } packages;

  wisp_basics_t symbols;
} wisp_t;

typedef enum wisp_tag {
  WISP_TAG_SYMBOL,
  WISP_TAG_FIXNUM,
  WISP_TAG_CONS,
  WISP_TAG_VECTOR,
  WISP_TAG_BYTES
} wisp_tag_t;

typedef struct wisp_cons {
  wisp_ptr_t car;
  wisp_ptr_t cdr;
} wisp_cons_t;

struct wisp_value {
  wisp_tag_t tag;
  wisp_ptr_t car;
  wisp_ptr_t cdr;
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

typedef enum wisp_error {
  WISP_ERROR_HEAP_FULL,
  WISP_ERROR_NOT_IMPLEMENTED
} wisp_error_t;

const char *wisp_error_names[] = {
  "HEAP-FULL",
  "NOT-IMPLEMENTED"
};

__inline__
static void debugger (void)
{
  __asm__ volatile ("int $0x03");
}

__attribute__ ((noreturn))
void
wisp_crash (wisp_t *ctx, wisp_error_t error)
{
  debugger ();
  fprintf (stderr, "wisp crash: %s\n", wisp_error_names[error]);
  exit (1);
}

__attribute__ ((noreturn))
void
wisp_not_implemented (wisp_t *ctx)
{
  wisp_crash (ctx, WISP_ERROR_NOT_IMPLEMENTED);
}

wisp_ptr_t
wisp_alloc (wisp_t *ctx, size_t n)
{
  if (wisp_free_space (ctx) < n)
    wisp_tidy (ctx);

  if (wisp_free_space (ctx) < n)
    wisp_crash (ctx, WISP_ERROR_HEAP_FULL);

  void *pointer = ctx->live_heap_tail;

  ctx->live_heap_tail += n;

#if WISP_DEBUG_ALLOC
  fprintf (stderr, "; wisp: alloc %lu %p\n", n, pointer);
#endif

  return pointer - ctx->live_heap;
}

#define WISP_REF(ctx, x) \
  ((wisp_value_t *) ((ctx)->live_heap + (x)))

#define WISP_REF_I(ctx, x, i) \
  ((wisp_value_t *) \
   ((ctx)->live_heap + (x) + (i) * sizeof (wisp_fixnum_t)))

// #define WISP_VECTOR_REF(ctx, vector, index) (WISP_REF (ctx, (vector))[index])

wisp_ptr_t
wisp_alloc_value (wisp_t *ctx, wisp_tag_t tag)
{
  wisp_ptr_t value =
    wisp_alloc (ctx, sizeof (wisp_value_t));

  (WISP_REF (ctx, value))->tag = tag;

  return value;
}

wisp_ptr_t
wisp_cons (wisp_t *ctx,
           wisp_ptr_t car,
           wisp_ptr_t cdr)
{
  wisp_ptr_t value =
    wisp_alloc_value (ctx, WISP_TAG_CONS);

  WISP_REF (ctx, value)->car = car;
  WISP_REF (ctx, value)->cdr = cdr;

  return value;
}

wisp_ptr_t
wisp_vector (wisp_t *ctx,
             wisp_fixnum_t entries,
             ...)
{
  va_list args;
  va_start (args, entries);

  wisp_ptr_t value =
    wisp_alloc_value (ctx, WISP_TAG_VECTOR);

  wisp_ptr_t data =
    wisp_alloc (ctx, entries * sizeof (wisp_value_t));

  wisp_fixnum_t *data_ptr = ctx->live_heap + data;

  for (int i = 0; i < entries; i++)
    data_ptr[i] = va_arg (args, wisp_fixnum_t);

  va_end (args);

  WISP_REF (ctx, value)->car = entries;
  WISP_REF (ctx, value)->cdr = data;

  return value;
}

wisp_fixnum_t
wisp_fixnum (wisp_t *ctx,
             wisp_fixnum_t fixnum)
{
  wisp_ptr_t value = wisp_alloc_value (ctx, WISP_TAG_FIXNUM);

  WISP_REF (ctx, value)->car = fixnum;
  WISP_REF (ctx, value)->cdr = 0;

  return value;
}


wisp_ptr_t
wisp_string_n (wisp_t *ctx,
               const char *start,
               int length)
{
  wisp_ptr_t value =
    wisp_alloc_value (ctx, WISP_TAG_BYTES);

  WISP_REF (ctx, value)->car = length;
  WISP_REF (ctx, value)->cdr = wisp_alloc (ctx, length + 1);

  char *buffer = (char *) WISP_REF (ctx, WISP_REF (ctx, value)->cdr);

  buffer[length] = 0;
  memcpy (buffer, start, length);

  return value;
}

wisp_ptr_t
wisp_string (wisp_t *ctx,
             const char *string)
{
  size_t length = strlen (string);

  return wisp_string_n (ctx, string, length);
}

void
wisp_assert (wisp_t *ctx,
             bool condition,
             const char *message)
{
  if (!condition)
    {
      fprintf (stderr, "wisp assertion failed: %s\n", message);
      exit (1);
    }
}

#define WISP_ASSERT(ctx, test) (wisp_assert (ctx, test, #test))


void
wisp_vector_set (wisp_t *ctx,
                 wisp_ptr_t vector,
                 int index,
                 wisp_ptr_t value)
{
  wisp_ptr_t *x = ctx->live_heap + vector + index * sizeof (wisp_ptr_t);
  *x = value;
}

bool
wisp_equal (wisp_t *ctx,
            wisp_ptr_t a,
            wisp_ptr_t b)
{
  wisp_value_t *aa = WISP_REF (ctx, a);
  wisp_value_t *bb = WISP_REF (ctx, b);

  wisp_tag_t tag = aa->tag;

  if (tag != bb->tag)
    return false;

  switch (tag)
    {
    case WISP_TAG_FIXNUM:
      return aa->car == bb->car;

    case WISP_TAG_BYTES:
      return aa->car == bb->car
        && !memcmp (ctx->live_heap + aa->cdr,
                    ctx->live_heap + bb->cdr,
                    aa->car);

    default:
      wisp_not_implemented (ctx);
    }
}

wisp_ptr_t
wisp_intern (wisp_t *ctx,
             wisp_ptr_t package,
             wisp_ptr_t name)
{
  WISP_ASSERT (ctx, WISP_REF (ctx, package)->tag == WISP_TAG_VECTOR);
  WISP_ASSERT (ctx, WISP_REF (ctx, package)->car == 3);
  // WISP_ASSERT (ctx, WISP_VECTOR_REF (package, 0) == ctx->symbols.PACKAGE);

  wisp_ptr_t symbols = WISP_REF (ctx, package)->cdr + 2;
  wisp_ptr_t current = symbols;

  while (WISP_REF (ctx, current)->tag == WISP_TAG_CONS)
    {
      wisp_ptr_t symbol = WISP_REF (ctx, current)->car;

      if (wisp_equal (ctx, WISP_REF (ctx, symbol)->car, name))
        return symbol;

      current = WISP_REF (ctx, current)->cdr;
    }

  wisp_ptr_t symbol = wisp_alloc_value (ctx, WISP_TAG_SYMBOL);

  fprintf (stderr, "; wisp: new symbol: %s\n", (char *) (WISP_REF (ctx, WISP_REF (ctx, name)->cdr)));

  WISP_REF (ctx, symbol)->car = name;

  wisp_ptr_t *xs = ctx->live_heap + package;
  xs[2] = wisp_cons (ctx, symbol, symbols);

  return symbol;
}

wisp_ptr_t
wisp_symbol (wisp_t *ctx,
             const char *name)
{
  fprintf (stderr, "; wisp: new symbol: %s\n", name);

  wisp_ptr_t value =
    wisp_alloc_value (ctx, WISP_TAG_SYMBOL);

  WISP_REF (ctx, value)->car = wisp_string (ctx, name);
  WISP_REF (ctx, value)->cdr = 0;

  return value;
}

wisp_ptr_t
wisp_nil (wisp_t *ctx)
{
  return ctx->symbols.NIL;
}

wisp_ptr_t
wisp_package (wisp_t *ctx,
              wisp_ptr_t name)
{
  return
    wisp_vector (ctx, 3,
                 ctx->symbols.PACKAGE,
                 name,
                 wisp_nil (ctx));
}

#define WISP_INTERN(ctx, package, name) \
  (ctx)->symbols.name = \
    wisp_intern (ctx, (ctx)->packages.package, wisp_string (ctx, #name))

wisp_t
wisp_start (size_t heap_size)
{
  uint8_t *live_heap = malloc (heap_size);
  uint8_t *idle_heap = malloc (heap_size);

  size_t heap_megabytes = heap_size / (1024 * 1024);

  fprintf (stderr, "; wisp: live heap %p (%lu MB)\n",
           live_heap, heap_megabytes);

  fprintf (stderr, "; wisp: idle heap %p (%lu MB)\n",
           idle_heap, heap_megabytes);

  wisp_t ctx = (wisp_t) {
    .heap_size = heap_size,
    .live_heap = live_heap,
    .live_heap_tail = live_heap,
    .idle_heap = idle_heap,
  };

  // Make these symbols manually; intern doesn't work yet.
  ctx.symbols.NIL = wisp_symbol (&ctx, "NIL");
  ctx.symbols.PACKAGE = wisp_symbol (&ctx, "PACKAGE");

  ctx.packages.LISP =
    wisp_package (&ctx, wisp_string (&ctx, "LISP"));

  // Put NIL and PACKAGE in the symbol list of the LISP package.
  wisp_ptr_t *symbols = ctx.live_heap + ctx.packages.LISP;
  symbols[2] =
    wisp_cons (&ctx, ctx.symbols.NIL,
               wisp_cons (&ctx, ctx.symbols.PACKAGE, ctx.symbols.NIL));

  WISP_INTERN (&ctx, LISP, QUOTE);
  WISP_INTERN (&ctx, LISP, T);
  WISP_INTERN (&ctx, LISP, DONE);

  return ctx;
}

void
wisp_stop (wisp_t *ctx)
{
  free (ctx->live_heap);
  free (ctx->idle_heap);
  fprintf (stderr, "; wisp: stop\n");
}

// S-expression terms are just Wisp values on the Wisp heap; there's
// no separate data type for syntax.  Aside from terms, the virtual
// machine state has scopes and contexts.  Let's represent those also
// as Wisp values on the Wisp heap.

typedef struct wisp_machine {
  wisp_ptr_t term;
  wisp_ptr_t scopes;
  wisp_ptr_t plan;
} wisp_machine_t;

bool
wisp_is_constant_variable (wisp_t *ctx,
                           wisp_ptr_t term)
{
  if (term == ctx->symbols.NIL)
    return true;

  if (term == ctx->symbols.T)
    return true;

  return false;
}

bool
wisp_is_term_irreducible (wisp_t *ctx,
                          wisp_ptr_t term)
{
  switch (WISP_REF (ctx, term)->tag)
    {
    case WISP_TAG_FIXNUM:
    case WISP_TAG_VECTOR:
    case WISP_TAG_BYTES:
      return true;

    case WISP_TAG_SYMBOL:
      return wisp_is_constant_variable (ctx, term);

    case WISP_TAG_CONS:
      return WISP_REF (ctx, term)->car == ctx->symbols.QUOTE;
    }
}

wisp_ptr_t
wisp_step (wisp_t *ctx,
           wisp_machine_t *machine)
{
  wisp_ptr_t term = machine->term;
  wisp_ptr_t plan = machine->plan;

  if (wisp_is_term_irreducible (ctx, term))
    {
      if (plan == ctx->symbols.DONE)
        return term;

      wisp_not_implemented (ctx);
    }

  wisp_not_implemented (ctx);
}

wisp_ptr_t
wisp_read (wisp_t *ctx, const char **stream);

wisp_ptr_t
wisp_read_list (wisp_t *ctx,
                const char **stream)
{
  char c = **stream;

  if (c == ')')
    return wisp_nil (ctx);

  wisp_ptr_t car =
    wisp_read (ctx, stream);

  wisp_ptr_t cdr =
    wisp_read_list (ctx, stream);

  return wisp_cons (ctx, car, cdr);
}

wisp_ptr_t
wisp_read_symbol (wisp_t *ctx,
                  const char **stream)
{
  const char *after = *stream + 1;

  while (isalpha (*after))
    ++after;

  int length = after - *stream;

  wisp_ptr_t name =
    wisp_string_n (ctx, *stream, length);

  uint8_t *string = ctx->live_heap + WISP_REF (ctx, name)->cdr;

  for (int i = 0; i < length; i++)
    string[i] = toupper (string[i]);

  *stream = after;

  return wisp_intern (ctx, ctx->packages.LISP, name);
}

wisp_ptr_t
wisp_read_fixnum (wisp_t *ctx,
                  const char **stream)
{
  const char *after = *stream + 1;

  while (isdigit (*after))
    ++after;

  int length = after - *stream;

  if (length > 10)
    wisp_not_implemented (ctx);

  char digits[length];

  for (int i = 0; i < length; i++)
    digits[i] = (*stream)[i];

  digits[length] = 0;

  *stream = after;

  return wisp_fixnum (ctx, atoi (digits));
}

wisp_ptr_t
wisp_read (wisp_t *ctx,
           const char **stream)
{
  char c = **stream;

  while (isspace (**stream))
    ++*stream;

  c = **stream;

  if (c == 0)
    wisp_not_implemented (ctx);

  if (c == '(')
    {
      ++*stream;
      return wisp_read_list (ctx, stream);
    }

  if (isalpha (c))
    return wisp_read_symbol (ctx, stream);

  if (isdigit (c))
    return wisp_read_fixnum (ctx, stream);

  wisp_not_implemented (ctx);
}

void
wisp_dump (wisp_t *ctx,
           wisp_ptr_t ptr)
{
  wisp_value_t *value = WISP_REF (ctx, ptr);

  switch (value->tag)
    {
    case WISP_TAG_FIXNUM:
      printf ("%d", value->car);
      break;

    case WISP_TAG_BYTES:
      printf ("\"%.*s\"", value->car, (char *) (ctx->live_heap + value->cdr));
      break;

    case WISP_TAG_SYMBOL:
      printf ("%.*s", WISP_REF (ctx, value->car)->car, (char *) ctx->live_heap + WISP_REF (ctx, value->car)->cdr);
      break;

    case WISP_TAG_CONS:
      printf ("(");
      wisp_dump (ctx, value->car);
      if (value->cdr == wisp_nil (ctx))
        printf (")");
      else
        {
          printf (" ");
          wisp_dump (ctx, value->cdr);
        }

      break;

    default:
      wisp_not_implemented (ctx);
    }
}

void
wisp_dump_heap (wisp_t *ctx)
{
  printf ("wisp heap v0\n");
  printf ("size %lu\n", ctx->heap_size);
  printf ("tail %lu\n", ctx->live_heap_tail - ctx->live_heap);
  printf ("package LISP %u\n", ctx->packages.LISP);
  printf ("heap ");
  for (void *x = ctx->live_heap; x < ctx->live_heap_tail; x++)
    printf ("%02x", *((char *) x));
  printf ("\n");
}

int
main (int argc, char **argv)
{
  fprintf (stderr, "\n; wisp: starting\n");

  wisp_t ctx = wisp_start (4 * 1024 * 1024);

  const char *foo_s = "(let ((x 1)) x)";

  wisp_ptr_t foo = wisp_read (&ctx, &foo_s);

  wisp_dump (&ctx, foo);
  printf ("\n");

  wisp_dump_heap (&ctx);

  wisp_stop (&ctx);
}
