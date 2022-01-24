//
// This is a Lisp interpreter that compiles to WebAssembly.
//
// Wisp supports first-class delimited continuations.
//
// The Wisp execution state is serializable like in Smalltalk.
//

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int32_t wisp_fixnum_t;

typedef struct wisp_value wisp_value_t;

typedef struct wisp_basics {
  wisp_value_t *NIL;
  wisp_value_t *PACKAGE;
  wisp_value_t *QUOTE;
  wisp_value_t *T;

  wisp_value_t *DONE;
} wisp_basics_t;

typedef struct wisp {
  size_t heap_size;

  void *live_heap;
  void *live_heap_tail;
  void *idle_heap;

  struct {
    wisp_value_t *LISP;
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
  wisp_value_t *car;
  wisp_value_t *cdr;
} wisp_cons_t;

struct wisp_value {
  wisp_tag_t tag;

  union {
    wisp_fixnum_t fixnum;
    wisp_value_t *car;
    uint8_t byte;
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

typedef enum wisp_error {
  WISP_ERROR_HEAP_FULL,
  WISP_ERROR_NOT_IMPLEMENTED
} wisp_error_t;

const char *wisp_error_names[] = {
  "HEAP-FULL",
  "NOT-IMPLEMENTED"
};

__attribute__ ((noreturn))
void
wisp_crash (wisp_t *ctx, wisp_error_t error)
{
  fprintf (stderr, "wisp crash: %s\n", wisp_error_names[error]);
  exit (1);
}

__attribute__ ((noreturn))
void
wisp_not_implemented (wisp_t *ctx)
{
  wisp_crash (ctx, WISP_ERROR_NOT_IMPLEMENTED);
}

void *
wisp_alloc (wisp_t *ctx, size_t n)
{
  if (wisp_free_space(ctx) < n)
    wisp_tidy (ctx);

  if (wisp_free_space(ctx) < n)
    wisp_crash (ctx, WISP_ERROR_HEAP_FULL);

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
wisp_string_n (wisp_t *ctx,
               const char *start,
               int length)
{
  wisp_value_t *value =
    wisp_alloc (ctx, sizeof (wisp_tag_t) + length);

  value->tag = WISP_TAG_BYTES;

  memcpy (&value->byte, start, length);

  return value;
}

wisp_value_t *
wisp_string (wisp_t *ctx,
             const char *string)
{
  size_t length = strlen (string);

  wisp_value_t *value =
    wisp_alloc (ctx, sizeof (wisp_tag_t) + length);

  value->tag = WISP_TAG_BYTES;

  memcpy (&value->byte, string, length);

  return value;
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

#define WISP_VECTOR_REF(vector, index) (*(&(vector)->cdr + (index)))

wisp_value_t *
wisp_vector_get (wisp_t *ctx,
                 wisp_value_t *vector,
                 int index)
{
  return WISP_VECTOR_REF (vector, index);
}

void
wisp_vector_set (wisp_t *ctx,
                 wisp_value_t *vector,
                 int index,
                 wisp_value_t *value)
{
  WISP_VECTOR_REF (vector, index) = value;
}

bool
wisp_equal (wisp_t *ctx,
            wisp_value_t *a,
            wisp_value_t *b)
{
  wisp_tag_t tag = a->tag;

  if (tag != b->tag)
    return false;

  switch (tag)
    {
    case WISP_TAG_FIXNUM:
      return a->fixnum == b->fixnum;

    case WISP_TAG_BYTES:
      return a->fixnum == b->fixnum
        && !memcmp (&a->byte, &b->byte, a->fixnum);

    default:
      wisp_not_implemented (ctx);
    }
}

wisp_value_t *
wisp_intern (wisp_t *ctx,
             wisp_value_t *package,
             wisp_value_t *name)
{
  WISP_ASSERT (ctx, package->tag == WISP_TAG_VECTOR);
  WISP_ASSERT (ctx, package->fixnum == 3);
  WISP_ASSERT (ctx, WISP_VECTOR_REF (package, 0) == ctx->symbols.PACKAGE);

  wisp_value_t *symbols = WISP_VECTOR_REF (package, 2);
  wisp_value_t *current = symbols;

  while (current->tag == WISP_TAG_CONS)
    {
      wisp_value_t *symbol = current->car;

      if (wisp_equal (ctx, symbol->car, name))
        return symbol;

      current = symbol->cdr;
    }

  wisp_value_t *symbol = wisp_alloc_1 (ctx, WISP_TAG_SYMBOL);

  symbol->car = name;

  WISP_VECTOR_REF (package, 2) =
    wisp_cons (ctx, symbol, symbols);

  return symbol;
}

wisp_value_t *
wisp_symbol (wisp_t *ctx,
             const char *name)
{
  wisp_value_t *value =
    wisp_alloc_1 (ctx, WISP_TAG_SYMBOL);

  value->car = wisp_string (ctx, name);

  return value;
}

wisp_value_t *
wisp_nil (wisp_t *ctx)
{
  return ctx->symbols.NIL;
}

wisp_value_t *
wisp_package (wisp_t *ctx,
              wisp_value_t *name)
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

  fprintf (stderr, "wisp: live heap %p (%lu MB)\n",
           live_heap, heap_megabytes);

  fprintf (stderr, "wisp: idle heap %p (%lu MB)\n",
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
  WISP_VECTOR_REF (ctx.packages.LISP, 2) =
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
  fprintf (stderr, "wisp: stop\n");
}

// S-expression terms are just Wisp values on the Wisp heap; there's
// no separate data type for syntax.  Aside from terms, the virtual
// machine state has scopes and contexts.  Let's represent those also
// as Wisp values on the Wisp heap.

typedef struct wisp_machine {
  wisp_value_t *term;
  wisp_value_t *scopes;
  wisp_value_t *plan;
} wisp_machine_t;

bool
wisp_is_constant_variable (wisp_t *ctx,
                           wisp_value_t *term)
{
  if (term == ctx->symbols.NIL)
    return true;

  if (term == ctx->symbols.T)
    return true;

  return false;
}

bool
wisp_is_term_irreducible (wisp_t *ctx,
                          wisp_value_t *term)
{
  switch (term->tag)
    {
    case WISP_TAG_FIXNUM:
    case WISP_TAG_VECTOR:
    case WISP_TAG_BYTES:
      return true;

    case WISP_TAG_SYMBOL:
      return wisp_is_constant_variable (ctx, term);

    case WISP_TAG_CONS:
      return term->car == ctx->symbols.QUOTE;
    }
}

wisp_value_t *
wisp_step (wisp_t *ctx,
           wisp_machine_t *machine)
{
  wisp_value_t *term = machine->term;
  wisp_value_t *plan = machine->plan;

  if (wisp_is_term_irreducible (ctx, term))
    {
      if (plan == ctx->symbols.DONE)
        return term;

      wisp_not_implemented (ctx);
    }

  wisp_not_implemented (ctx);
}

wisp_value_t *
wisp_read (wisp_t *ctx, const char **stream);

wisp_value_t *
wisp_read_list (wisp_t *ctx,
                const char **stream)
{
  char c = **stream;

  if (c == ')')
    return wisp_nil (ctx);

  wisp_value_t *car =
    wisp_read (ctx, stream);

  wisp_value_t *cdr =
    wisp_read_list (ctx, stream);

  return wisp_cons (ctx, car, cdr);
}

wisp_value_t *
wisp_read_symbol (wisp_t *ctx,
                  const char **stream)
{
  const char *after = *stream + 1;

  while (isalpha (*after))
    ++after;

  int length = after - *stream;

  wisp_value_t *name =
    wisp_string_n (ctx, *stream, length);

  uint8_t *string = &name->byte;

  for (int i = 0; i < length; i++)
    string[i] = toupper (string[i]);

  *stream = after;

  return wisp_intern (ctx, ctx->packages.LISP, name);
}

wisp_value_t *
wisp_read (wisp_t *ctx,
           const char **stream)
{
  char c = **stream;

  if (c == 0)
    wisp_not_implemented (ctx);

  if (c == '(')
    {
      ++*stream;
      return wisp_read_list (ctx, stream);
    }

  if (isalpha (c))
    {
      return wisp_read_symbol (ctx, stream);
    }

  wisp_not_implemented (ctx);
}

void
wisp_dump (wisp_t *ctx,
           wisp_value_t *value)
{
  switch (value->tag)
    {
    case WISP_TAG_FIXNUM:
      printf ("%d", value->fixnum);
      break;

    case WISP_TAG_BYTES:
      printf ("\"%.*s\"", value->fixnum, &value->byte);
      break;

    case WISP_TAG_SYMBOL:
      printf ("%.*s", value->car->fixnum, &value->car->byte);
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

int
main (int argc, char **argv)
{
  wisp_t ctx = wisp_start (4 * 1024 * 1024);

  const char *foo_s = "(let ((x 1)) x)";

  wisp_value_t *foo = wisp_read (&ctx, &foo_s);

  wisp_dump (&ctx, foo);

  wisp_stop (&ctx);
}
