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

#define WISP_LOWTAG_MASK 0x7
#define WISP_LOWTAG(x) ((x) & WISP_LOWTAG_MASK)

#define WISP_ALIGNED_PROPERLY(x) (((x) & WISP_LOWTAG_MASK) == 0)

wisp_word_t
wisp_align (wisp_word_t x)
{
  return (x + WISP_LOWTAG_MASK + 1) & ~WISP_LOWTAG_MASK;
}

typedef enum wisp_lowtag {
  WISP_LOWTAG_FIXNUM_0,
  WISP_LOWTAG_FUNCTION_PTR,
  WISP_LOWTAG_OTHER_IMMEDIATE_0,
  WISP_LOWTAG_LIST_PTR,
  WISP_LOWTAG_FIXNUM_1,
  WISP_LOWTAG_STRUCT_PTR,
  WISP_LOWTAG_OTHER_IMMEDIATE_1,
  WISP_LOWTAG_OTHER_PTR,
} wisp_lowtag_t;

#define WISP_WIDETAG_MASK 0xff

#define WISP_WIDETAG_STRING 0x32
#define WISP_WIDETAG_SYMBOL 0xAE
#define WISP_WIDETAG_INSTANCE 0xC2

#define WISP_IS_FIXNUM(x) (((x) & 3) == 0)
#define WISP_IS_OTHER_IMMEDIATE(x) (((x) & 3) == 2)
#define WISP_IS_OTHER_PTR(x) (((x) & 7) == 7)
#define WISP_IS_PTR(x) ((x) & 1)
#define WISP_IS_LIST(x) (((x) & WISP_LOWTAG_MASK) == WISP_LOWTAG_LIST_PTR)
#define WISP_IS_STRUCT(x) (((x) & WISP_LOWTAG_MASK) == WISP_LOWTAG_STRUCT_PTR)
#define WISP_IS_STRING(x) (((x) & WISP_WIDETAG_MASK) == WISP_WIDETAG_STRING)

static void *heap;
static size_t heap_size = 1024 * 1024 * 4;
static size_t heap_used = 0;

static const wisp_word_t NIL =
  WISP_LOWTAG_LIST_PTR;

static wisp_word_t PACKAGE = -1;
static wisp_word_t COMMON_LISP = -1;

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
  fprintf (stderr, "; wisp: alloc %x %d\n", tag, size);
#endif

  uint32_t i = heap_used;
  heap_used += size;

  return i ^ tag;
}

#define WISP_WORD_SIZE 4
#define WISP_HEADER_WORD_SIZE (2 * WISP_WORD_SIZE)
#define WISP_CONS_SIZE (2 * WISP_WORD_SIZE)
#define WISP_SYMBOL_SIZE (wisp_align (5 * WISP_WORD_SIZE))

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
wisp_header_word (uint32_t data,
                  uint8_t widetag)
{
  assert ((data << 8) >> 8 == data);
  assert ((widetag & 3) == 2);

  return (data << 8) | widetag;
}

wisp_word_t
wisp_read (const char **s);

wisp_word_t
wisp_read_list (const char **stream)
{
  char c = **stream;

  if (c == ')')
    return NIL;

  wisp_word_t car = wisp_read (stream);
  wisp_word_t cdr = wisp_read_list (stream);

  return wisp_cons (car, cdr);
}

#define WISP_PACKAGE_HEADER (wisp_header_word (3, WISP_WIDETAG_INSTANCE))
#define WISP_SYMBOL_HEADER (wisp_header_word (5, WISP_WIDETAG_SYMBOL))

wisp_word_t
wisp_make_package (wisp_word_t name)
{
  // A package is a STRUCT-PTR to a 3-word instance block:
  //
  // type    PACKAGE
  // slot 1  PACKAGE-NAME
  // slot 2  PACKAGE-EXTERNAL-SYMBOLS

  wisp_word_t ptr =
    wisp_alloc_raw (wisp_align (4 * WISP_WORD_SIZE),
                    WISP_LOWTAG_STRUCT_PTR);

  wisp_word_t *header = wisp_deref (ptr);

  header[0] = WISP_PACKAGE_HEADER;
  header[1] = PACKAGE;
  header[2] = name;
  header[3] = NIL;

  return ptr;
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

  wisp_word_t widetag = a & WISP_WIDETAG_MASK;

  if (widetag != (b & WISP_WIDETAG_MASK))
    return false;

  switch (widetag)
    {
    case WISP_WIDETAG_STRING:
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

  return symbol;
}

wisp_word_t
wisp_intern_symbol (wisp_word_t name,
                    wisp_word_t package)
{
  wisp_word_t *package_header =
    wisp_deref (package);

  assert (package_header[0] == WISP_PACKAGE_HEADER);
  assert (package_header[1] == PACKAGE);

  wisp_word_t cur = package_header[3];

  assert (WISP_IS_LIST (cur));

  while (cur != NIL)
    {
      wisp_word_t *cons = wisp_deref (cur);
      wisp_word_t car = cons[0];
      wisp_word_t cdr = cons[1];

      wisp_word_t *symbol_header = wisp_deref (car);
      wisp_word_t symbol_name = symbol_header[4];

      if (wisp_equal (car, name))
        return car;

      cur = cdr;
    }

  // No symbol found; create it.
  wisp_word_t symbol = wisp_create_symbol (name);
  wisp_word_t *symbol_header = wisp_deref (symbol);

  symbol_header[5] = package;

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

  while (isalpha (*after++));

  int length = after - *stream;

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

  int length = after - *stream;

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

  wisp_not_implemented ();
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
  words[4] = 0xdead0000;
  words[5] = wisp_string ("NIL");
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
}

void
wisp_dump (wisp_word_t word)
{
  if (WISP_IS_FIXNUM (word))
    printf ("%d", word >> 2);

  else if (word == NIL)
    printf ("NIL");

  else if (WISP_IS_LIST (word))
    {
      putchar ('(');

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
          else if (WISP_IS_LIST (cdr))
            {
              putchar (' ');
              word = cdr;
            }
          else
            {
              printf (" . ");
              wisp_dump (word);
              break;
            }
        }

      putchar (')');
    }

  else if (WISP_IS_STRUCT (word))
    {
      printf ("#<STRUCT ");

      wisp_word_t *struct_header = wisp_deref (word);

      wisp_dump (struct_header[1]);
      printf (" @%x>", word & ~WISP_LOWTAG_MASK);
    }

  else if (WISP_IS_OTHER_PTR (word))
    {
      wisp_word_t *header = wisp_deref (word);
      if (header[0] == WISP_SYMBOL_HEADER)
        {
          wisp_word_t *string_header = wisp_deref (header[4]);
          printf ("%s", wisp_string_buffer (string_header));
        }
      else
        wisp_not_implemented ();
    }

  else
    {
      printf ("{word tag %x}\n", word & WISP_WIDETAG_MASK);

      wisp_not_implemented ();
    }
}

int
main ()
{
  printf (";; Wisp starting.\n");

  wisp_start ();

  const char *example = "(defun foo () 1 2 3)";

  wisp_dump (wisp_read (&example));

  wisp_crash ("foo");

  return 0;
}
