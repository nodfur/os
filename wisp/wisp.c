#define WISP_DEBUG_ALLOC 0

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

wisp_word_t
wisp_align (wisp_word_t x)
{
  return (x + WISP_LOWTAG_MASK + 1) & ~WISP_LOWTAG_MASK;
}

static void *heap;
static size_t heap_size = 1024 * 1024 * 4;
static size_t heap_used = 0;

static const wisp_word_t NIL =
  WISP_LOWTAG_LIST_PTR;

static wisp_word_t PACKAGE = -1;
static wisp_word_t COMMON_LISP = -1;
static wisp_word_t QUOTE = -1;
static wisp_word_t APPLY = -1;
static wisp_word_t EVAL = -1;
static wisp_word_t SCOPE = -1;
static wisp_word_t CLOSURE = -1;
static wisp_word_t LAMBDA = -1;
static wisp_word_t MACRO = -1;
static wisp_word_t PARAMS = -1;
static wisp_word_t SET_SYMBOL_FUNCTION = -1;

wisp_word_t
wisp_fixnum (int32_t x)
{
  return x << 2;
}

wisp_word_t
wisp_alloc_raw (wisp_word_t size, wisp_lowtag_t tag)
{
  assert (WISP_ALIGNED_PROPERLY (size));
  assert (heap_used + size < heap_size);

#if WISP_DEBUG_ALLOC
  fprintf (stderr, "; alloc %x %d\n", tag, size);
#endif

  uint32_t i = heap_used;
  heap_used += size;

  return i ^ tag;
}

#define WISP_WORD_SIZE 4
#define WISP_HEADER_WORD_SIZE (2 * WISP_WORD_SIZE)
#define WISP_CONS_SIZE (2 * WISP_WORD_SIZE)
#define WISP_SYMBOL_SIZE (wisp_align (6 * WISP_WORD_SIZE))

wisp_word_t *
wisp_deref (wisp_word_t ptr)
{
  return heap + (ptr & ~WISP_LOWTAG_MASK);
}

wisp_word_t
wisp_cons (wisp_word_t car, wisp_word_t cdr)
{
  wisp_word_t cons =
    wisp_alloc_raw (WISP_CONS_SIZE, WISP_LOWTAG_LIST_PTR);

  wisp_word_t *data = wisp_deref (cons);
  data[0] = car;
  data[1] = cdr;

  return cons;
}

wisp_word_t
wisp_car (wisp_word_t list)
{
  return (wisp_deref (list))[0];
}

wisp_word_t
wisp_cdr (wisp_word_t list)
{
  return (wisp_deref (list))[1];
}

wisp_word_t
wisp_header_word (uint32_t data,
                  uint8_t widetag)
{
  assert ((data << 8) >> 8 == data);
  assert ((widetag & 3) == 2);

  return (data << 8) | widetag;
}

wisp_word_t
wisp_header_word_data (uint32_t header)
{
  return header >> 8;
}

wisp_word_t
wisp_read (const char **s);

wisp_word_t
wisp_read_list (const char **stream)
{
  char c = **stream;

  if (c == ')')
    {
      ++*stream;
      return NIL;
    }

  wisp_word_t car = wisp_read (stream);
  wisp_word_t cdr = wisp_read_list (stream);

  return wisp_cons (car, cdr);
}

#define WISP_INSTANCE_HEADER(n) \
  (wisp_header_word ((n) + 1, WISP_WIDETAG_INSTANCE))

#define WISP_SYMBOL_HEADER \
  (wisp_header_word (WISP_SYMBOL_SIZE, WISP_WIDETAG_SYMBOL))


wisp_word_t
wisp_alloc_words (int words,
                  wisp_word_t lowtag)
{
  return wisp_alloc_raw (wisp_align (WISP_WORD_SIZE * words), lowtag);
}

wisp_word_t
wisp_make_instance_with_slots (wisp_word_t type,
                               int n_slots,
                               wisp_word_t *slots)
{
  wisp_word_t pointer =
    wisp_alloc_words (2 + n_slots,
                      WISP_LOWTAG_STRUCT_PTR);

  wisp_word_t *header =
    wisp_deref (pointer);

  header[0] = WISP_INSTANCE_HEADER (n_slots);
  header[1] = type;

  for (int i = 0; i < n_slots; i++)
    header[2 + i] = slots[i];

  return pointer;
}

void wisp_dump (wisp_word_t x);

wisp_word_t
wisp_make_instance (wisp_word_t type,
                    int n_slots,
                    ...)
{
  va_list args;
  va_start (args, n_slots);

  wisp_word_t slots[n_slots];

  for (int i = 0; i < n_slots; i++)
    slots[i] = va_arg (args, wisp_word_t);

  va_end (args);

  return
    wisp_make_instance_with_slots (type, n_slots, slots);
}

wisp_word_t
wisp_make_package (wisp_word_t name)
{
  return
    wisp_make_instance (PACKAGE, 2, name, NIL);
}

char *
wisp_string_buffer (wisp_word_t *header)
{
  return (char *) &(header[2]);
}

__inline__
static void debugger (void)
{
  __asm__ volatile ("int $0x03");
}

__attribute__ ((noreturn))
void
wisp_crash (const char *error)
{
  fprintf (stderr, "wisp crash: %s\n", error);
  debugger ();
  exit (1);
}

__attribute__ ((noreturn))
void
wisp_not_implemented ()
{
  wisp_crash ("not implemented");
}

bool
wisp_equal (wisp_word_t a,
            wisp_word_t b)
{
  if (a == b)
    return true;

  wisp_word_t lowtag = a & WISP_LOWTAG_MASK;
  wisp_word_t lowtag_b = b & WISP_LOWTAG_MASK;

  wisp_word_t widetag = a & WISP_WIDETAG_MASK;
  wisp_word_t widetag_b = b & WISP_WIDETAG_MASK;

  /* fprintf (stderr, ";; comparing %d (%d) and %d (%d)\n", */
  /*          a, lowtag, b, lowtag_b); */

  switch (lowtag)
    {
    case WISP_LOWTAG_OTHER_PTR:
      {
        /* fprintf (stderr, ";; comparing strings\n"); */
        wisp_word_t *a_header = wisp_deref (a);
        wisp_word_t *b_header = wisp_deref (b);

        if (a_header[0] != b_header[0])
          return false;

        int size = a_header[0] >> 8;

        if (strncmp (wisp_string_buffer (a_header),
                     wisp_string_buffer (b_header),
                     size))
          return false;

        return true;
     }

    default:
      wisp_not_implemented ();
    }
}

wisp_word_t
wisp_create_symbol (wisp_word_t name)
{
  wisp_word_t symbol =
    wisp_alloc_raw (WISP_SYMBOL_SIZE, WISP_LOWTAG_OTHER_PTR);

  wisp_word_t *symbol_header =
    wisp_deref (symbol);

  symbol_header[0] = WISP_SYMBOL_HEADER;
  symbol_header[1] = NIL;
  symbol_header[2] = 0xdead0003;
  symbol_header[3] = NIL;
  symbol_header[4] = name;
  symbol_header[5] = NIL;
  symbol_header[6] = NIL;

  return symbol;
}

wisp_word_t
wisp_intern_symbol (wisp_word_t name,
                    wisp_word_t package)
{
  /* fprintf (stderr, */
  /*          "; %s", */
  /*          wisp_string_buffer (wisp_deref (name))); */

  wisp_word_t *package_header =
    wisp_deref (package);

  assert (package_header[0] == WISP_INSTANCE_HEADER (2));
  assert (package_header[1] == PACKAGE);

  wisp_word_t cur = package_header[3];

  assert (WISP_IS_LIST_PTR (cur));

  while (cur != NIL)
    {
      wisp_word_t *cons = wisp_deref (cur);
      wisp_word_t car = cons[0];
      wisp_word_t cdr = cons[1];

      wisp_word_t *symbol_header = wisp_deref (car);
      wisp_word_t symbol_name = symbol_header[4];

      if (wisp_equal (symbol_name, name))
        {
          /* fprintf (stderr, " found %d\n", car); */
          return car;
        }

      cur = cdr;
    }

  // No symbol found; create it.
  wisp_word_t symbol = wisp_create_symbol (name);
  wisp_word_t *symbol_header = wisp_deref (symbol);

  symbol_header[5] = package;
  package_header[3] = wisp_cons (symbol, package_header[3]);

  /* fprintf (stderr, " created %d\n", symbol); */

  return symbol;
}

wisp_word_t
wisp_string_n (const char *source, int length)
{
  // Get an OTHER-PTR pointing to a string block with an extra byte
  // for an extra zero terminator.
  wisp_word_t string_ptr =
    wisp_alloc_raw
    (wisp_align (WISP_HEADER_WORD_SIZE + length + 1),
     WISP_LOWTAG_OTHER_PTR);

  // Get a C pointer into the Wisp heap by word.
  wisp_word_t *header = wisp_deref (string_ptr);

  // Get a C byte pointer to the string's buffer memory.
  char *buffer = wisp_string_buffer (header);

  // Set the header's length (excluding zero sentinel) and type tag.
  header[0] = wisp_header_word (length, WISP_WIDETAG_STRING);

  // Copy the source characters into the string buffer.
  memcpy (buffer, source, length);

  // Insert the zero sentinel just after the data.
  buffer[length] = 0;

  // Return the OTHER-PTR.
  return string_ptr;
}

wisp_word_t
wisp_string (const char *source)
{
  return wisp_string_n (source, strlen (source));
}

wisp_word_t
wisp_read_symbol (const char **stream)
{
  const char *after = *stream + 1;

  while (isalpha (*after) || (*after == '-'))
    ++after;

  int length = ++after - *stream - 1;

  wisp_word_t name =
    wisp_string_n (*stream, length);

  char *data =
    wisp_string_buffer (wisp_deref (name));

  for (int i = 0; i < length; i++)
    data[i] = toupper (data[i]);

  *stream = after - 1;

  return wisp_intern_symbol (name, COMMON_LISP);
}

wisp_word_t
wisp_read_fixnum (const char **stream)
{
  const char *after = *stream + 1;

  while (isdigit (*after++));

  int length = after - *stream - 1;

  if (length > 10)
    wisp_not_implemented ();

  char digits[length];

  for (int i = 0; i < length; i++)
    digits[i] = (*stream)[i];

  digits[length] = 0;

  *stream = after - 1;

  return wisp_fixnum (atoi (digits));
}

wisp_word_t
wisp_read (const char **stream)
{
  while (isspace (**stream))
    ++*stream;

  char c = **stream;

  if (c == 0)
    wisp_not_implemented ();

  if (c == '(')
    {
      ++*stream;
      return wisp_read_list (stream);
    }

  if (isalpha (c))
    return wisp_read_symbol (stream);

  if (isdigit (c))
    return wisp_read_fixnum (stream);

  if (c == '\'')
    {
      ++*stream;
      return wisp_cons (QUOTE,
                        wisp_cons (wisp_read (stream), NIL));
    }

  wisp_not_implemented ();
}


wisp_word_t
wisp_intern_lisp (const char *name)
{
  return wisp_intern_symbol (wisp_string (name), COMMON_LISP);
}

void
wisp_start ()
{
  heap = malloc (heap_size);

  wisp_word_t *words = heap;

  words[0] = 6 << 8;
  words[1] = WISP_WIDETAG_SYMBOL;
  words[2] = NIL;
  words[3] = NIL;
  words[4] = wisp_string ("NIL");
  words[5] = 0xdead0000;
  words[6] = 0xdead0002;

  heap_used = 6 * sizeof *words;

  PACKAGE =
    wisp_create_symbol (wisp_string ("PACKAGE"));

  COMMON_LISP =
    wisp_make_package (wisp_string ("COMMON-LISP"));

  wisp_word_t *common_lisp_header =
    wisp_deref (COMMON_LISP);

  common_lisp_header[3] =
    wisp_cons (NIL, NIL);

  common_lisp_header[3] =
    wisp_cons (PACKAGE, common_lisp_header[3]);

  QUOTE   = wisp_intern_lisp ("QUOTE");
  APPLY   = wisp_intern_lisp ("APPLY");
  EVAL    = wisp_intern_lisp ("EVAL");
  SCOPE   = wisp_intern_lisp ("SCOPE");
  CLOSURE = wisp_intern_lisp ("CLOSURE");
}

bool
wisp_is_quote (wisp_word_t word)
{
  if (!WISP_IS_LIST_PTR (word))
    return false;

  return (wisp_deref (word))[0] == QUOTE;
}

void
wisp_dump (wisp_word_t word)
{
  if (WISP_IS_FIXNUM (word))
    fprintf (stderr, "%d", word >> 2);

  else if (word == NIL)
    fprintf (stderr, "NIL");

  else if (wisp_is_quote (word))
    {
      wisp_word_t quoted = wisp_car (wisp_cdr (word));
      fprintf (stderr, "'");
      wisp_dump (quoted);
    }

  else if (WISP_IS_LIST_PTR (word))
    {
      fprintf (stderr, "(");

      while (word != NIL)
        {
          wisp_word_t *cons = wisp_deref (word);
          wisp_word_t  car = cons[0];
          wisp_word_t  cdr = cons[1];

          wisp_dump (car);

          if (cdr == NIL)
            {
              break;
            }
          else if (WISP_IS_LIST_PTR (cdr))
            {
              fprintf (stderr, " ");
              word = cdr;
            }
          else
            {
              fprintf (stderr, " . ");
              wisp_dump (word);
              break;
            }
        }

      fprintf (stderr, ")");
    }

  else if (WISP_IS_STRUCT_PTR (word))
    {
      fprintf (stderr, "#<STRUCT ");

      wisp_word_t *struct_header = wisp_deref (word);

      wisp_dump (struct_header[1]);
      fprintf (stderr, " @%x>", word & ~WISP_LOWTAG_MASK);
    }

  else if (WISP_IS_OTHER_PTR (word))
    {
      wisp_word_t *header = wisp_deref (word);
      if (header[0] == WISP_SYMBOL_HEADER)
        {
          wisp_word_t *string_header = wisp_deref (header[4]);
          fprintf (stderr, "%s", wisp_string_buffer (string_header));
        }
      else
        wisp_not_implemented ();
    }

  else
    {
      fprintf (stderr, "{%d}\n", word);
    }
}

typedef struct {
  bool value;
  wisp_word_t term;
  wisp_word_t scopes;
  wisp_word_t plan;
} wisp_machine_t;

wisp_word_t *
wisp_is_symbol (wisp_word_t value)
{
  if (!WISP_IS_OTHER_PTR (value))
    return 0;

  wisp_word_t *header = wisp_deref (value);

  if (header[0] == WISP_SYMBOL_HEADER)
    return header;
  else
    return 0;
}

bool
wisp_term_irreducible (wisp_word_t term)
{
  switch (term & WISP_LOWTAG_MASK)
    {
    case WISP_LOWTAG_FIXNUM_0:
    case WISP_LOWTAG_FIXNUM_1:
      return true;

    default:
      if (term == NIL)
        return true;

      if (WISP_IS_LIST_PTR (term))
        return false;

      if (WISP_IS_STRUCT_PTR (term))
        return true;

      if (wisp_is_symbol (term))
        return false;

      wisp_crash ("strange term");
    }
}

wisp_word_t
wisp_struct_header_type (wisp_word_t *header)
{
  return header[1];
}

wisp_word_t *
wisp_is_instance (wisp_word_t word,
                  wisp_word_t type)
{
  if (WISP_IS_STRUCT_PTR (word))
    {
      wisp_word_t *header = wisp_deref (word);
      if (wisp_struct_header_type (header) == type)
        return header;
    }

  return 0;
}

wisp_word_t
wisp_make_apply_plan (wisp_word_t callee,
                      wisp_word_t values,
                      wisp_word_t terms,
                      wisp_word_t scopes,
                      wisp_word_t next)
{
  return wisp_make_instance
    (APPLY, 5, callee, values, terms, scopes, next);
}

typedef struct __attribute__ ((__packed__)) {
  wisp_word_t callee;
  wisp_word_t values;
  wisp_word_t terms;
  wisp_word_t scopes;
  wisp_word_t next;
} wisp_apply_plan_t;

typedef struct __attribute__ ((__packed__)) {
  wisp_word_t params;
  wisp_word_t body;
  wisp_word_t scopes;
  wisp_word_t macro;
} wisp_closure_t;

wisp_apply_plan_t *
wisp_get_apply_plan (wisp_word_t *header)
{
  return (wisp_apply_plan_t *) (header + 2);
}

wisp_closure_t *
wisp_ensure_function (wisp_word_t value)
{
  wisp_word_t *header =
    wisp_is_instance (value, CLOSURE);

  return (wisp_closure_t *) (header + 2);
}

wisp_closure_t *
wisp_get_closure (wisp_word_t value)
{
  wisp_word_t *symbol_header = wisp_is_symbol (value);

  wisp_word_t *closure_header =
    symbol_header
    ? wisp_is_instance (symbol_header[6], CLOSURE)
    : wisp_is_instance (value, CLOSURE);

  if (closure_header)
    return (wisp_closure_t *) (closure_header + 2);
  else
    wisp_crash ("not a function");
}

int
wisp_length (wisp_word_t list)
{
  int i = 0;

  while (list != NIL)
    {
      list = (wisp_deref (list))[1];
      ++i;
    }

  return i;
}

wisp_word_t
wisp_lambda_list_to_params (wisp_word_t lambda_list)
{
  int length = wisp_length (lambda_list);
  wisp_word_t slots[length];

  /* fprintf (stderr, ";; %d ", length); */
  /* wisp_dump (lambda_list); */
  /* fprintf (stderr, "\n"); */

  for (int i = 0; i < length; i++)
    {
      wisp_word_t *cons = wisp_deref (lambda_list);
      slots[i] = cons[0];
      lambda_list = cons[1];
    }

  wisp_word_t params =
    wisp_make_instance_with_slots (PARAMS, length, slots);

  return params;
}

void
wisp_set_symbol_function (wisp_word_t symbol,
                          wisp_word_t value)
{
  wisp_word_t *header = wisp_deref (symbol);
  header[6] = value;

  fprintf (stderr, "; setting function ");
  wisp_dump (symbol);
  fprintf (stderr, ": ");
  wisp_dump (value);
  fprintf (stderr, "\n");
}

wisp_machine_t
wisp_step_into_function (wisp_machine_t *machine,
                         bool backwards,
                         wisp_apply_plan_t *apply_plan)
{
  // We've evaluated all the subforms of a function application.
  // Now switch the machine term to the closure body, the machine
  // scopes to the closure scopes extended with the bound parameters,
  // and the machine plan to the continuation of the apply plan.

  wisp_closure_t *closure =
    wisp_get_closure (apply_plan->callee);

  wisp_word_t *parameter_struct =
    wisp_deref (closure->params);

  wisp_word_t parameter_count =
    wisp_header_word_data (parameter_struct[0]) - 1;

  /* fprintf (stderr, "; stepping into "); */
  /* wisp_dump (apply_plan->callee); */
  /* fprintf (stderr, " %d\n", parameter_count); */

  wisp_word_t *parameter_names =
    parameter_struct + 2;

  wisp_word_t scope_slots[2 * parameter_count];

  wisp_word_t values =
    apply_plan->values;

  for (int i = 0; i < parameter_count; i++)
    {
      int parameter_index = backwards
        ? parameter_count - i - 1
        : i;

      assert (values != NIL);

      wisp_word_t *cons = wisp_deref (values);
      wisp_word_t car = cons[0];
      wisp_word_t cdr = cons[1];

      scope_slots[parameter_index * 2] =
        parameter_names[parameter_index];

      scope_slots[parameter_index * 2 + 1] =
        car;

      values = cdr;
    }

  wisp_word_t parameter_scope =
    wisp_make_instance_with_slots
    (SCOPE, 2 * parameter_count, scope_slots);

  if (WISP_WIDETAG (closure->body) == WISP_WIDETAG_BUILTIN)
    {
      wisp_word_t builtin = closure->body >> 8;
      /* fprintf (stderr, ";; builtin %d\n", builtin); */

      switch (builtin)
        {
        case WISP_BUILTIN_LAMBDA:
          {
            wisp_word_t lambda_list = scope_slots[1];
            wisp_word_t lambda_body = scope_slots[3];

            wisp_word_t lambda_params =
              wisp_lambda_list_to_params (lambda_list);

            wisp_word_t closure = wisp_make_instance
              (CLOSURE, 4,
               lambda_params,
               lambda_body,
               machine->scopes,
               NIL);

            return (wisp_machine_t) {
              .term = closure,
              .value = true,
              .scopes = machine->scopes,
              .plan = apply_plan->next
            };
          }

        case WISP_BUILTIN_MACRO:
          {
            wisp_word_t macro_list = scope_slots[1];
            wisp_word_t macro_body = scope_slots[3];

            wisp_word_t macro_params =
              wisp_lambda_list_to_params (macro_list);

            wisp_word_t closure = wisp_make_instance
              (CLOSURE, 4,
               macro_params,
               macro_body,
               machine->scopes,
               MACRO);

            return (wisp_machine_t) {
              .term = closure,
              .value = true,
              .scopes = machine->scopes,
              .plan = apply_plan->next
            };
          }

        case WISP_BUILTIN_SET_SYMBOL_FUNCTION:
          {
            wisp_set_symbol_function
              (scope_slots[1], scope_slots[3]);

            return (wisp_machine_t) {
              .term = scope_slots[3],
              .value = false,
              .scopes = machine->scopes,
              .plan = apply_plan->next
            };
          }

        case WISP_BUILTIN_CONS:
          {
            return (wisp_machine_t) {
              .term = wisp_cons (scope_slots[1], scope_slots[3]),
              .value = true,
              .scopes = machine->scopes,
              .plan = apply_plan->next,
            };
          }

        default:
          wisp_crash ("unknown builtin");
        }
    }

  if (closure->macro == NIL)
    return (wisp_machine_t) {
      .term = closure->body,
      .value = false,
      .scopes = wisp_cons (parameter_scope, closure->scopes),
      .plan = apply_plan->next
    };

  else
    {
      wisp_word_t eval_plan =
        wisp_make_instance (EVAL, 2,
                            machine->scopes,
                            apply_plan->next);

      return (wisp_machine_t) {
        .term = closure->body,
        .value = false,
        .scopes = wisp_cons (parameter_scope, closure->scopes),
        .plan = eval_plan
      };
    }

}

bool
wisp_find_binding_in_scope (wisp_word_t scope,
                            wisp_word_t symbol,
                            wisp_word_t *result)
{
  wisp_word_t *header = wisp_deref (scope);
  wisp_word_t *slots = header + 2;
  int size = wisp_header_word_data (header[0]) / 2;
  for (int i = 0; i < size; i++)
    {
      wisp_word_t variable = slots[i * 2];
      if (variable == symbol)
        {
          *result = slots[i * 2 + 1];
          return true;
        }
    }

  return false;
}

bool
wisp_find_binding (wisp_word_t scopes,
                   wisp_word_t symbol,
                   wisp_word_t *result)
{
  if (scopes == NIL)
    return false;

  wisp_word_t *scopes_cons = wisp_deref (scopes);

  wisp_word_t scopes_car = scopes_cons[0];
  wisp_word_t scopes_cdr = scopes_cons[1];

  if (wisp_find_binding_in_scope (scopes_car, symbol, result))
    return true;

  return wisp_find_binding (scopes_cdr, symbol, result);
}

bool
wisp_step (wisp_machine_t *machine)
{
  wisp_word_t term = machine->term;
  wisp_word_t scopes = machine->scopes;
  wisp_word_t plan = machine->plan;

  /* if (machine->value) */
  /*   fprintf (stderr, "; value: "); */
  /* else */
  /*   fprintf (stderr, "; term: "); */
  /* wisp_dump (term); */
  /* printf ("\n"); */

  /* fprintf (stderr, "; plan: "); */
  /* wisp_dump (plan); */
  /* printf ("\n"); */

  if (machine->value || wisp_term_irreducible (term))
    {
      if (plan == NIL)
        return false;

      else if (WISP_IS_STRUCT_PTR (plan))
        {
          wisp_word_t *header = wisp_deref (plan);
          wisp_word_t type = wisp_struct_header_type (header);

          if (type == APPLY)
            {
              wisp_apply_plan_t *apply_plan =
                wisp_get_apply_plan (header);

              wisp_word_t terms = apply_plan->terms;
              if (terms == NIL)
                {
                  wisp_word_t new_apply_plan =
                    wisp_make_apply_plan
                    (apply_plan->callee,
                     wisp_cons (term, apply_plan->values),
                     NIL,
                     apply_plan->scopes,
                     apply_plan->next);

                  *machine = wisp_step_into_function
                    (machine,
                     true,
                     wisp_get_apply_plan
                     (wisp_deref (new_apply_plan)));

                  return true;
                }
              else
                {
                  wisp_word_t *cons =
                    wisp_deref (terms);

                  wisp_word_t car = cons[0];
                  wisp_word_t cdr = cons[1];

                  wisp_word_t next_apply_plan =
                    wisp_make_apply_plan
                    (apply_plan->callee,
                     wisp_cons (term, apply_plan->values),
                     cdr,
                     apply_plan->scopes,
                     apply_plan->next);

                  machine->term = car;
                  machine->value = false;
                  machine->plan = next_apply_plan;

                  return true;
                }
            }

          else if (type == EVAL)
            {
              /* fprintf (stderr, "; eval\n"); */
              machine->value = false;
              machine->scopes = header[2];
              machine->plan = header[3];
              return true;
            }
        }
    }

  else if (WISP_IS_LIST_PTR (term))
    {
      wisp_word_t *cons = wisp_deref (term);
      wisp_word_t car = cons[0];
      wisp_word_t cdr = cons[1];

      if (car == QUOTE)
        {
          machine->term = wisp_car (cdr);
          machine->value = true;
          return true;
        }

      else
        {
          // Evaluate a function call (f x y z).
          if (WISP_IS_OTHER_PTR (car))
            {
              wisp_word_t *header = wisp_deref (car);
              if (header[0] == WISP_SYMBOL_HEADER)
                {
                  if (cdr == NIL)
                    {
                      // XXX: It's not necessary to allocate a plan on
                      // the heap here.

                      wisp_word_t apply_plan =
                        wisp_make_apply_plan
                        (car, NIL, NIL, scopes, plan);

                      *machine =
                        wisp_step_into_function
                        (machine,
                         false,
                         wisp_get_apply_plan
                         (wisp_deref (apply_plan)));

                      return true;
                    }
                  else
                    {
                      wisp_closure_t *closure =
                        wisp_get_closure (car);

                      if (closure->macro == NIL)
                        {
                          wisp_word_t *term_list = wisp_deref (cdr);
                          wisp_word_t first_term = term_list[0];
                          wisp_word_t remaining_terms = term_list[1];

                          wisp_word_t apply_plan = wisp_make_apply_plan
                            (car, NIL, remaining_terms, scopes, plan);

                          machine->term = first_term;
                          machine->value = false;
                          machine->plan = apply_plan;

                          return true;
                        }
                      else
                        {
                          wisp_word_t apply_plan = wisp_make_apply_plan
                            (car, cdr, NIL, scopes, plan);

                          *machine =
                            wisp_step_into_function
                            (machine,
                             false,
                             wisp_get_apply_plan
                             (wisp_deref (apply_plan)));

                          return true;
                        }
                    }
                }

            }
        }
    }

  else if (wisp_is_symbol (term))
    {
      wisp_word_t binding;

      /* wisp_dump (machine->scopes); */

      if (wisp_find_binding (machine->scopes, term, &binding))
        {
          machine->term = binding;
          machine->value = true;
          return true;
        }

      else
        wisp_crash ("unbound variable");
    }

  wisp_crash ("bad term");
}

wisp_machine_t
wisp_initial_machine (wisp_word_t term)
{
  return (wisp_machine_t) {
    .term = term,
    .plan = NIL,
    .scopes = NIL
  };
}

wisp_word_t
wisp_make_list (int count,
                ...)
{
  wisp_word_t elements[count];

  va_list args;
  va_start (args, count);

  for (int i = 0; i < count; i++)
    elements[i] = va_arg (args, wisp_word_t);

  va_end (args);

  wisp_word_t list = NIL;

  for (int i = 0; i < count; i++)
    list = wisp_cons (elements[count - i - 1], list);

  return list;
}

void
wisp_setup (void)
{
  LAMBDA =
    wisp_intern_lisp ("LAMBDA");
  MACRO =
    wisp_intern_lisp ("MACRO");
  PARAMS =
    wisp_intern_lisp ("PARAMS");
  SET_SYMBOL_FUNCTION =
    wisp_intern_lisp ("SET-SYMBOL-FUNCTION");

  wisp_set_symbol_function
    (LAMBDA,
     wisp_make_instance
     (CLOSURE, 4,
      wisp_lambda_list_to_params
      (wisp_make_list (2,
                       wisp_intern_lisp ("LAMBDA-LIST"),
                       wisp_intern_lisp ("LAMBDA-BODY"))),
      (WISP_BUILTIN_LAMBDA << 8) | WISP_WIDETAG_BUILTIN,
      NIL,
      MACRO));

  wisp_set_symbol_function
    (MACRO,
     wisp_make_instance
     (CLOSURE, 4,
      wisp_lambda_list_to_params
      (wisp_make_list (2,
                       wisp_intern_lisp ("MACRO-LIST"),
                       wisp_intern_lisp ("MACRO-BODY"))),
      (WISP_BUILTIN_MACRO << 8) | WISP_WIDETAG_BUILTIN,
      NIL,
      MACRO));

  wisp_set_symbol_function
    (SET_SYMBOL_FUNCTION,
     wisp_make_instance
     (CLOSURE, 4,
      wisp_lambda_list_to_params
      (wisp_make_list (2,
                       wisp_intern_lisp ("SYMBOL"),
                       wisp_intern_lisp ("CLOSURE"))),
      (WISP_BUILTIN_SET_SYMBOL_FUNCTION << 8) | WISP_WIDETAG_BUILTIN,
      NIL,
      NIL));

  wisp_set_symbol_function
    (wisp_intern_lisp ("CONS"),
     wisp_make_instance
     (CLOSURE, 4,
      wisp_lambda_list_to_params
      (wisp_make_list (2,
                       wisp_intern_lisp ("CAR"),
                       wisp_intern_lisp ("CDR"))),
      (WISP_BUILTIN_CONS << 8) | WISP_WIDETAG_BUILTIN,
      NIL,
      NIL));

}

wisp_word_t
wisp_eval_code (const char *code)
{
  wisp_word_t term = wisp_read (&code);

  /* fprintf (stderr, "\n> "); */

  /* wisp_dump (term); */
  /* fprintf (stderr, "\n"); */

  wisp_machine_t machine =
    wisp_initial_machine (term);

  while (wisp_step (&machine))
    ;

  /* fprintf (stderr, "=> "); */
  /* wisp_dump (machine.term); */
  /* fprintf (stderr, "\n"); */

  return machine.term;
}

int
main ()
{
  wisp_start ();
  wisp_setup ();

  wisp_eval_code
    ("(set-symbol-function 'identity (lambda (x) x))");

  wisp_eval_code
    (
     "(set-symbol-function"
     " 'defun"
     " (macro"
     "  (name params body)"
     "  (cons 'set-symbol-function"
     "        (cons (cons 'quote (cons name nil))"
     "              (cons (cons 'lambda"
     "                          (cons params"
     "                                (cons body nil))) nil)))))"
     );

  wisp_eval_code
    (
     "(set-symbol-function"
     " 'defmacro"
     " (macro"
     "  (name params body)"
     "  (cons 'set-symbol-function"
     "        (cons (cons 'quote (cons name nil))"
     "              (cons (cons 'macro"
     "                          (cons params"
     "                                (cons body nil))) nil)))))"
     );

  /* wisp_eval_code */
  /*   ("(defun foo (x y) x)"); */

  /* wisp_eval_code */
  /*   ("(foo 'x 'y)"); */

  while (true)
    {
      printf ("> ");
      size_t n = 0;
      char *buffer = 0;
      if (getline (&buffer, &n, stdin) != -1)
        {
          wisp_word_t result = wisp_eval_code (buffer);
          wisp_dump (result);
          printf ("\n");
          free (buffer);
        }
      else
        {
          printf ("\n");
          return 0;
        }
    }

  return 0;
}
