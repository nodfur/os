#include <stdio.h>
#include <string.h>

#define WISP_DEBUG_ALLOC 0

#include "wisp.h"

void *heap;

#define MEGABYTES (1024 * 1024)

size_t heap_size = 4 * MEGABYTES;
size_t heap_used = 0;

wisp_word_t APPLY;
wisp_word_t CLOSURE;
wisp_word_t WISP;
wisp_word_t EVAL;
wisp_word_t LAMBDA;
wisp_word_t MACRO;
wisp_word_t PACKAGE;
wisp_word_t PARAMS;
wisp_word_t QUOTE;
wisp_word_t SCOPE;
wisp_word_t SET_SYMBOL_FUNCTION;

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
  WISP_DEBUG ("alloc %x %d\n", tag, size);
#endif

  uint32_t i = heap_used;
  heap_used += size;

  return i ^ tag;
}

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
#ifndef __EMSCRIPTEN__
  __asm__ volatile ("int $0x03");
#endif
}

__attribute__ ((noreturn))
void
wisp_crash (const char *error)
{
  WISP_DEBUG ("crash: %s\n", error);
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

  /* WISP_DEBUG ("comparing %d (%d) and %d (%d)\n", */
  /*             a, lowtag, b, lowtag_b); */

  switch (lowtag)
    {
    case WISP_LOWTAG_OTHER_PTR:
      {
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
        return car;

      cur = cdr;
    }

  // No symbol found; create it.
  wisp_word_t symbol = wisp_create_symbol (name);
  wisp_word_t *symbol_header = wisp_deref (symbol);

  symbol_header[5] = package;
  package_header[3] = wisp_cons (symbol, package_header[3]);

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
wisp_intern_lisp (const char *name)
{
  return wisp_intern_symbol (wisp_string (name), WISP);
}

void
wisp_init ()
{
  fprintf (stderr, "; allocating %lu MB heap\n", heap_size / MEGABYTES);

  heap = malloc (heap_size);
}

void
wisp_allocate_heap ()
{
  fprintf (stderr, "; allocating %lu MB heap\n", heap_size / MEGABYTES);

  heap = calloc (heap_size, 1);
}

void
wisp_start ()
{
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

  WISP =
    wisp_make_package (wisp_string ("WISP"));

  WISP_DEBUG ("WISP 0x%X\n", WISP);

  wisp_word_t *common_lisp_header =
    wisp_deref (WISP);

  common_lisp_header[3] =
    wisp_cons (NIL, NIL);

  common_lisp_header[3] =
    wisp_cons (PACKAGE, common_lisp_header[3]);
}

void
wisp_intern_basic_symbols ()
{
  APPLY = wisp_intern_lisp ("APPLY");
  CLOSURE = wisp_intern_lisp ("CLOSURE");
  EVAL = wisp_intern_lisp ("EVAL");
  LAMBDA = wisp_intern_lisp ("LAMBDA");
  MACRO = wisp_intern_lisp ("MACRO");
  PACKAGE = wisp_intern_lisp ("PACKAGE");
  PARAMS = wisp_intern_lisp ("PARAMS");
  QUOTE = wisp_intern_lisp ("QUOTE");
  SCOPE = wisp_intern_lisp ("SCOPE");

  SET_SYMBOL_FUNCTION = wisp_intern_lisp ("SET-SYMBOL-FUNCTION");
}

bool
wisp_is_quote (wisp_word_t word)
{
  if (!WISP_IS_LIST_PTR (word))
    return false;

  return (wisp_deref (word))[0] == QUOTE;
}

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

wisp_closure_t *
wisp_ensure_function (wisp_word_t value)
{
  wisp_word_t *header =
    wisp_is_instance (value, CLOSURE);

  return (wisp_closure_t *) (header + 2);
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


void
wisp_set_symbol_function (wisp_word_t symbol,
                          wisp_word_t value)
{
  wisp_word_t *header = wisp_deref (symbol);
  header[6] = value;

  fprintf (stderr, "; FUNCTION ");
  wisp_dump (symbol);
  fprintf (stderr, " ← ");
  wisp_dump (value);
  fprintf (stderr, "\n");
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

wisp_word_t
wisp_simple_params (int count, ...)
{
  wisp_word_t elements[count];

  va_list args;
  va_start (args, count);

  for (int i = 0; i < count; i++)
    elements[i] =
      wisp_intern_lisp (va_arg (args, const char *));

  va_end (args);

  wisp_word_t list = NIL;

  for (int i = 0; i < count; i++)
    list = wisp_cons (elements[count - i - 1], list);

  return wisp_lambda_list_to_params (list);

}

void
wisp_builtin_function (wisp_word_t builtin_name,
                       wisp_word_t builtin_id,
                       wisp_word_t params)
{
  wisp_word_t closure =
    wisp_make_instance
    (CLOSURE, 4,
     params,
     (builtin_id << 8) | WISP_WIDETAG_BUILTIN,
     NIL,
     NIL);

  wisp_set_symbol_function (builtin_name, closure);
}

void
wisp_builtin_macro (wisp_word_t builtin_name,
                    wisp_word_t builtin_id,
                    wisp_word_t params)
{
  wisp_word_t closure =
    wisp_make_instance
    (CLOSURE, 4,
     params,
     (builtin_id << 8) | WISP_WIDETAG_BUILTIN,
     NIL,
     MACRO);

  wisp_set_symbol_function (builtin_name, closure);
}

void
wisp_setup (void)
{
  wisp_builtin_macro
    (LAMBDA,
     WISP_BUILTIN_LAMBDA,
     wisp_simple_params (2, "PARAMS", "BODY"));

  wisp_builtin_macro
    (MACRO,
     WISP_BUILTIN_MACRO,
     wisp_simple_params (2, "PARAMS", "BODY"));

  wisp_builtin_function
    (SET_SYMBOL_FUNCTION,
     WISP_BUILTIN_SET_SYMBOL_FUNCTION,
     wisp_simple_params (2, "SYMBOL", "CLOSURE"));

  wisp_builtin_function
    (wisp_intern_lisp ("CONS"),
     WISP_BUILTIN_CONS,
     wisp_simple_params (2, "CAR", "CDR"));
}

WISP_EXPORT
wisp_word_t
wisp_eval_code (const char *code)
{
  wisp_word_t term = wisp_read (&code);

  wisp_machine_t machine =
    wisp_initial_machine (term);

  while (wisp_step (&machine))
    ;

  return machine.term;
}

void
wisp_save_image (const char *path)
{
  FILE *f = fopen (path, "w+");
  if (fwrite (heap, 1, heap_used, f) != heap_used)
    wisp_crash ("heap save write failed");
  WISP_DEBUG ("saved heap to %s\n", path);
  fclose (f);
}

void
wisp_load_image (const char *path)
{
  WISP_DEBUG ("loading heap from %s\n", path);
  FILE *f = fopen (path, "r");

  fseek (f, 0, SEEK_END);
  long size = ftell(f);
  fseek (f, 0, SEEK_SET);

  memset (heap, 0, heap_size);

  if (fread (heap, 1, size, f) != size)
    wisp_crash ("heap load read failed");

  heap_used = size;

  WISP = 0x6D;

#define RESTORE(x) x = wisp_intern_lisp (#x)

  WISP_DEBUG ("caching basic symbols\n");
  RESTORE (QUOTE);
  RESTORE (APPLY);
  RESTORE (EVAL);
  RESTORE (SCOPE);
  RESTORE (CLOSURE);
  RESTORE (LAMBDA);
  RESTORE (MACRO);
  RESTORE (PARAMS);
  RESTORE (SET_SYMBOL_FUNCTION);

#undef RESTORE
}

WISP_EXPORT
void
wisp_boot ()
{
  wisp_allocate_heap ();
  wisp_start ();
  wisp_intern_basic_symbols ();
  wisp_setup ();
}

int
main (int argc, char **argv)
{
  bool loading_heap = false;

  if (argc > 1 && strcmp (argv[1], "--load-heap") == 0)
    loading_heap = true;

  wisp_allocate_heap ();

  if (!loading_heap)
    {
      WISP_DEBUG ("starting system sans heap image\n");
      wisp_start ();
      wisp_intern_basic_symbols ();
      wisp_setup ();
    }
  else
    {
      WISP_DEBUG ("starting system from heap image\n");
      wisp_intern_basic_symbols ();
    }

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

  wisp_save_image ("wisp.heap");

  if (argc > 1 && strcmp (argv[1], "repl") == 0)
    {
      while (true)
        {
          printf ("> ");
          fflush (stdout);
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
    }

  return 0;
}
