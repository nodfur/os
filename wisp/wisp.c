#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define WISP_DEBUG_ALLOC 0

#include "wisp.h"

void *heap_base;
void *heap;
int old_heap;
int new_heap;
int new_heap_used;
int new_heap_scan;

#define MEGABYTES (1024 * 1024)

size_t heap_size = 4 * MEGABYTES;
/* size_t heap_used = 0; */

wisp_word_t wisp_cache[wisp_cache_size];

wisp_word_t
wisp_fixnum (int32_t x)
{
  return x << 2;
}

wisp_word_t
wisp_alloc_raw (wisp_word_t size, wisp_lowtag_t tag)
{
  assert (WISP_ALIGNED_PROPERLY (size));

  if (new_heap_used + size >= heap_size)
    {
      wisp_tidy ();
      assert (new_heap_used + size < heap_size);
    }

#if WISP_DEBUG_ALLOC
  WISP_DEBUG ("alloc %x %d\n", tag, size);
#endif

  uint32_t i = new_heap_used;
  new_heap_used += size;

  return (new_heap + i) | tag;
}

wisp_word_t *
wisp_deref (wisp_word_t ptr)
{
  return heap + (ptr & ~WISP_LOWTAG_MASK);
}

wisp_word_t
wisp_header_word (uint32_t data, uint8_t widetag)
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
wisp_alloc_words (int words, wisp_word_t lowtag)
{
  return wisp_alloc_raw (wisp_align (WISP_WORD_SIZE * words), lowtag);
}

wisp_word_t
wisp_make_instance_with_slots (wisp_word_t type, int n_slots,
                               wisp_word_t *slots)
{
  wisp_word_t pointer
    = wisp_alloc_words (2 + n_slots, WISP_LOWTAG_STRUCT_PTR);

  wisp_word_t *header = wisp_deref (pointer);

  header[0] = WISP_INSTANCE_HEADER (n_slots);
  header[1] = type;

  for (int i = 0; i < n_slots; i++)
    header[2 + i] = slots[i];

  return pointer;
}

wisp_word_t
wisp_make_instance_va (wisp_word_t type, int n_slots, ...)
{
  va_list args;
  va_start (args, n_slots);

  wisp_word_t slots[n_slots];

  for (int i = 0; i < n_slots; i++)
    slots[i] = va_arg (args, wisp_word_t);

  va_end (args);

  return wisp_make_instance_with_slots (type, n_slots, slots);
}

wisp_word_t
wisp_make_package (wisp_word_t name)
{
  return wisp_make_instance_va
    (WISP_CACHE (PACKAGE), 2, name, NIL);
}

char *
wisp_string_buffer (wisp_word_t *header)
{
  return (char *)&(header[1]);
}

__inline__ static void
debugger (void)
{
#if defined(__x86_64__)
  __asm__ volatile("int $0x03");
#elif defined(EMSCRIPTEN)
  EM_ASM ({ debugger; });
#endif
}

__attribute__ ((noreturn)) void
wisp_crash (const char *error)
{
  WISP_DEBUG ("crash: %s\n", error);
  debugger ();
  exit (1);
}

__attribute__ ((noreturn)) void
wisp_not_implemented ()
{
  wisp_crash ("not implemented");
}

bool
wisp_equal (wisp_word_t a, wisp_word_t b)
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
                     wisp_string_buffer (b_header), size))
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
  wisp_word_t symbol
    = wisp_alloc_raw (wisp_align (WISP_WORD_SIZE * WISP_SYMBOL_SIZE),
                      WISP_LOWTAG_OTHER_PTR);

  wisp_word_t *symbol_header = wisp_deref (symbol);

  symbol_header[0] = WISP_SYMBOL_HEADER;
  symbol_header[1] = NIL;
  symbol_header[2] = NIL;
  symbol_header[3] = NIL;
  symbol_header[4] = name;
  symbol_header[5] = NIL;
  symbol_header[6] = NIL;

  return symbol;
}

wisp_word_t
wisp_intern_symbol (wisp_word_t name, wisp_word_t package)
{
  wisp_word_t *package_header = wisp_deref (package);

  assert (package_header[0] == WISP_INSTANCE_HEADER (2));
  assert (package_header[1] == WISP_CACHE (PACKAGE));

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
  wisp_word_t string_ptr
      = wisp_alloc_raw (wisp_align (WISP_WORD_SIZE + length + 1),
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
  return wisp_intern_symbol
    (wisp_string (name), WISP_CACHE (WISP));
}

void
wisp_allocate_heap ()
{
  WISP_DEBUG ("Allocating %lu MB heap\n", heap_size / MEGABYTES);

  heap_base = calloc (2 * heap_size, 1);
  old_heap = heap_size / 2;
  new_heap = 0;
  new_heap_scan = 0;
  heap = heap_base;
}

int wisp_static_space_size = 48;

void
wisp_start ()
{
  wisp_word_t *words = heap;

  new_heap_used = wisp_align (7 * sizeof *words);

  words[0] = WISP_SYMBOL_HEADER;
  words[1] = NIL;
  words[2] = NIL;
  words[3] = NIL;
  words[4] = wisp_string ("NIL");
  words[5] = NIL;
  words[6] = NIL;

  assert (new_heap_used == wisp_static_space_size);

  WISP_CACHE (PACKAGE) =
    wisp_create_symbol (wisp_string ("PACKAGE"));

  WISP_CACHE (WISP) =
    wisp_make_package (wisp_string ("WISP"));

  WISP_DEBUG ("Package WISP ← ");
  wisp_dump (stderr, WISP_CACHE (WISP));
  fprintf (stderr, "\n");

  wisp_word_t *common_lisp_header =
    wisp_deref (WISP_CACHE (WISP));

  common_lisp_header[3] = wisp_cons (NIL, NIL);

  WISP_DEBUG ("PACKAGE <<< 0x%x\n", WISP_CACHE (PACKAGE));

  common_lisp_header[3] =
    wisp_cons (WISP_CACHE (PACKAGE), common_lisp_header[3]);
}

void
wisp_intern_basic_symbols ()
{
  WISP_CACHE (T) = wisp_intern_lisp ("T");
  WISP_CACHE (APPLY) = wisp_intern_lisp ("APPLY");
  WISP_CACHE (CLOSURE) = wisp_intern_lisp ("CLOSURE");
  WISP_CACHE (EVAL) = wisp_intern_lisp ("EVAL");
  WISP_CACHE (LAMBDA) = wisp_intern_lisp ("LAMBDA");
  WISP_CACHE (MACRO) = wisp_intern_lisp ("MACRO");
  /* WISP_CACHE (PACKAGE) = wisp_intern_lisp ("PACKAGE"); */
  WISP_CACHE (PARAMS) = wisp_intern_lisp ("PARAMS");
  WISP_CACHE (QUOTE) = wisp_intern_lisp ("QUOTE");
  WISP_CACHE (SCOPE) = wisp_intern_lisp ("SCOPE");

  WISP_CACHE (SET_SYMBOL_FUNCTION) =
    wisp_intern_lisp ("SET-SYMBOL-FUNCTION");
}

bool
wisp_is_quote (wisp_word_t word)
{
  if (!WISP_IS_LIST_PTR (word))
    return false;

  return (wisp_deref (word))[0] == WISP_CACHE (QUOTE);
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
wisp_is_instance (wisp_word_t word, wisp_word_t type)
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
    wisp_is_instance (value, WISP_CACHE (CLOSURE));

  return (wisp_closure_t *)(header + 2);
}

int
wisp_length (wisp_word_t list)
{
  int i = 0;

  while (list != NIL)
    {
      list = wisp_cdr (list);
      ++i;
    }

  return i;
}

void
wisp_set_symbol_variable (wisp_word_t symbol, wisp_word_t value)
{
  wisp_word_t *header = wisp_deref (symbol);
  header[1] = value;

  WISP_DEBUG ("Variable ");
  wisp_dump (stderr, symbol);
  fprintf (stderr, " ← ");
  wisp_dump (stderr, value);
  fprintf (stderr, "\n");
}

wisp_machine_t
wisp_initial_machine (wisp_word_t term)
{
  return (wisp_machine_t){ .term = term, .plan = NIL, .scopes = NIL };
}

wisp_word_t
wisp_make_list (int count, ...)
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
    elements[i] = wisp_intern_lisp (va_arg (args, const char *));

  va_end (args);

  wisp_word_t list = NIL;

  for (int i = 0; i < count; i++)
    list = wisp_cons (elements[count - i - 1], list);

  return wisp_lambda_list_to_params (list);
}

wisp_defun_t wisp_builtins[WISP_N_BUILTINS];

void
wisp_register_builtin_defun
(wisp_defun_t defun)
{
  wisp_builtins[defun.id] = defun;

  wisp_set_symbol_function
    (wisp_intern_lisp (defun.name),
     (defun.id << 8) | WISP_WIDETAG_BUILTIN);
}

void
wisp_setup (void)
{
  wisp_set_symbol_variable (NIL, NIL);
}

WISP_EXPORT
wisp_word_t
wisp_eval_code (const char *code)
{
  /* WISP_DEBUG ("evaluating %s\n", code); */

  wisp_word_t term = wisp_read (&code);

  wisp_machine_t machine = wisp_initial_machine (term);
  wisp_machine = &machine;

  while (wisp_step (&machine))
    ;

  wisp_machine = NULL;

  return machine.term;
}

void
wisp_load_heap (const char *path)
{
  FILE *f = fopen (path, "r");

  wisp_allocate_heap ();

  if (fscanf (f, "WISP 0 %d\n", &(WISP_CACHE (WISP))) == 0)
    {
      wisp_crash ("heap load failed");
    }
  else
    {
      long start = ftell (f);

      fseek (f, 0, SEEK_END);
      long size = ftell (f) - start;
      fseek (f, start, SEEK_SET);

      WISP_DEBUG ("Loading heap from %s (%lu bytes)\n",
                  path, size);

      if (fread (heap, 1, size, f) != size)
        wisp_crash ("heap load read failed");

      new_heap_used = wisp_align (size);

      WISP_CACHE (PACKAGE) =
        wisp_deref (WISP_CACHE (WISP))[1];

      wisp_intern_basic_symbols ();
    }
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

void
wisp_stdlib ()
{
  wisp_eval_code ("(set-symbol-function 'identity (lambda (x) x))");

  wisp_eval_code (
      "(set-symbol-function"
      " 'defun"
      " (macro"
      "  (name params body)"
      "  (cons 'set-symbol-function"
      "        (cons (cons 'quote (cons name nil))"
      "              (cons (cons 'lambda"
      "                          (cons params"
      "                                (cons body nil))) nil)))))");

  wisp_eval_code (
      "(set-symbol-function"
      " 'defmacro"
      " (macro"
      "  (name params body)"
      "  (cons 'set-symbol-function"
      "        (cons (cons 'quote (cons name nil))"
      "              (cons (cons 'macro"
      "                          (cons params"
      "                                (cons body nil))) nil)))))");

  wisp_eval_code ("(defun tag (tag attrs body)"
                  "  (make-instance 'dom-element"
                  "    (cons tag (cons attrs (cons body nil)))))");
}

void wisp_defs (void);

WISP_EXPORT
void
wisp_start_without_heap ()
{
  WISP_DEBUG ("Starting system without heap image\n");
  wisp_start ();
  wisp_intern_basic_symbols ();
  wisp_setup ();
  wisp_defs ();
  wisp_stdlib ();
}

WISP_EXPORT
void
wisp_main ()
{
  char *heap_path = getenv ("WISP_HEAP");

  const char *target = "(unknown)";

#if defined(EMSCRIPTEN)
  target = "WebAssembly";
#elif defined(__x86_64__)
  target = "64-bit x86";
#endif

  WISP_DEBUG ("Welcome to Wisp for %s\n", target);

  if (!heap_path || access (heap_path, R_OK) != 0)
    {
      wisp_allocate_heap ();
      wisp_start_without_heap ();
    }
  else
    {
      wisp_load_heap (heap_path);
      wisp_defs ();
    }
}

WISP_EXPORT
void *
wisp_get_heap_pointer ()
{
  return heap;
}

int
main (int argc, char **argv)
{
#ifdef EMSCRIPTEN
  emscripten_exit_with_live_runtime ();
#else
  wisp_main ();

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
              wisp_dump (stdout, result);
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

  else if (argc > 2 && strcmp (argv[1], "-e") == 0)
    {
      wisp_word_t result = wisp_eval_code (argv[2]);
      wisp_dump (stdout, result);
      printf ("\n");
      wisp_tidy ();
    }
#endif

  return 0;
}

WISP_DEFQUOTE ("QUOTE", wisp_quote, 1)
(wisp_word_t term)
{
  return term;
}

WISP_DEFEVAL ("EVAL", wisp_eval, 1)
(wisp_word_t term)
{
  return term;
}

WISP_DEFMACRO ("LAMBDA", wisp_lambda, 2)
(wisp_word_t lambda_list, wisp_word_t body)
{
  wisp_word_t params = wisp_lambda_list_to_params (lambda_list);

  wisp_word_t closure = wisp_make_instance_va
    (WISP_CACHE (CLOSURE), 4,
     params, body, wisp_machine->scopes, NIL);

  return closure;
}

WISP_DEFMACRO ("MACRO", wisp_macro, 2)
(wisp_word_t lambda_list, wisp_word_t body)
{
  wisp_word_t params = wisp_lambda_list_to_params (lambda_list);

  wisp_word_t closure = wisp_make_instance_va
    (WISP_CACHE (CLOSURE), 4,
     params, body, wisp_machine->scopes, WISP_CACHE (MACRO));

  return closure;
}

WISP_DEFUN ("CONS", wisp_cons, 2)
(wisp_word_t car, wisp_word_t cdr)
{
  wisp_word_t cons
    = wisp_alloc_raw (WISP_CONS_SIZE, WISP_LOWTAG_LIST_PTR);

  wisp_word_t *data = wisp_deref (cons);
  data[0] = car;
  data[1] = cdr;

  return cons;
}

WISP_DEFUN ("CAR", wisp_car, 1)
(wisp_word_t list)
{
  return (wisp_deref (list))[0];
}

WISP_DEFUN ("CDR", wisp_cdr, 1)
(wisp_word_t list)
{
  return (wisp_deref (list))[1];
}

WISP_DEFUN ("MAKE-INSTANCE", wisp_make_instance, 2)
(wisp_word_t klass, wisp_word_t initargs)
{
  int length = wisp_length (initargs);
  wisp_word_t slots[length];

  wisp_word_t cons = initargs;
  for (int i = 0; i < length; i++) {
    slots[i] = wisp_car (cons);
    cons = wisp_cdr (cons);
  }

  return wisp_make_instance_with_slots
    (klass, length, slots);
}

WISP_DEFUN ("SET-SYMBOL-FUNCTION", wisp_set_symbol_function, 2)
(wisp_word_t symbol, wisp_word_t value)
{
  wisp_word_t *header = wisp_deref (symbol);
  header[6] = value;

  WISP_DEBUG ("Function ");
  wisp_dump (stderr, symbol);
  fprintf (stderr, " ← ");
  wisp_dump (stderr, value);
  fprintf (stderr, "\n");

  return value;
}

WISP_DEFUN ("COLLECT-GARBAGE", wisp_collect_garbage, 0)
(void)
{
  wisp_tidy ();
  return NIL;
}

WISP_DEFUN ("PRINT", wisp_print, 1)
(wisp_word_t x)
{
  wisp_dump (stdout, x);
  putchar ('\n');
  return x;
}

WISP_DEFUN ("SAVE-HEAP", wisp_save_heap, 1)
(wisp_word_t pathname)
{
  char *path = wisp_string_buffer (wisp_deref (pathname));

  WISP_DEBUG ("saving heap to %s\n", path);

  FILE *f = fopen (path, "w+");

  fprintf (f, "WISP 0 %d\n", WISP_CACHE (WISP));

  if (fwrite (heap, 1, new_heap_used, f) != new_heap_used)
    wisp_crash ("heap save write failed");
  WISP_DEBUG ("saved heap to %s\n", path);
  fclose (f);

#ifdef EMSCRIPTEN
  EM_ASM ({
    FS.syncfs (err => {
      if (err)
        {
          Module.printErr ('syncfs error');
          console.error (err);
        }
      else
        {
          console.error ('syncfs done');
        }
    });
  });
#endif

  return WISP_CACHE (T);
}

WISP_DEFUN ("+", wisp_add, 2)
(wisp_word_t x, wisp_word_t y)
{
  if (!WISP_IS_FIXNUM (x))
    wisp_crash ("not a number");

  if (!WISP_IS_FIXNUM (y))
    wisp_crash ("not a number");

  return x + y;
}

WISP_DEFUN ("-", wisp_subtract, 2)
(wisp_word_t x, wisp_word_t y)
{
  if (!WISP_IS_FIXNUM (x))
    wisp_crash ("not a number");

  if (!WISP_IS_FIXNUM (y))
    wisp_crash ("not a number");

  return x - y;
}

WISP_DEFUN ("*", wisp_multiply, 2)
(wisp_word_t x, wisp_word_t y)
{
  if (!WISP_IS_FIXNUM (x))
    wisp_crash ("not a number");

  if (!WISP_IS_FIXNUM (y))
    wisp_crash ("not a number");

  return ((x >> 2) * (y >> 2)) << 2;
}

void
wisp_defs (void)
{
  WISP_REGISTER (wisp_quote, "TERM");
  WISP_REGISTER (wisp_eval, "TERM");
  WISP_REGISTER (wisp_lambda, "LAMBDA-LIST", "BODY");
  WISP_REGISTER (wisp_macro, "LAMBDA-LIST", "BODY");
  WISP_REGISTER (wisp_cons, "CAR", "CDR");
  WISP_REGISTER (wisp_car, "CONS");
  WISP_REGISTER (wisp_cdr, "CONS");
  WISP_REGISTER (wisp_set_symbol_function, "SYMBOL", "FUNCTION");
  WISP_REGISTER (wisp_save_heap, "HEAP-PATH");
  WISP_REGISTER (wisp_collect_garbage, "COLLECT-GARBAGE");
  WISP_REGISTER (wisp_print, "PRINT");
  WISP_REGISTER (wisp_make_instance, "CLASS", "SLOTS");
  WISP_REGISTER (wisp_add, "X", "Y");
  WISP_REGISTER (wisp_subtract, "X", "Y");
  WISP_REGISTER (wisp_multiply, "X", "Y");
}
