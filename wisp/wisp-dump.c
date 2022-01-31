#include "wisp.h"

WISP_EXPORT
void
wisp_dump_stdout (wisp_word_t word)
{
  wisp_dump (stdout, word);
  printf ("\n");
}

void
wisp_dump (FILE *f, wisp_word_t word)
{
  if (WISP_IS_FIXNUM (word))
    fprintf (f, "%d", word >> 2);

  else if (word == NIL)
    fprintf (f, "NIL");

  else if (wisp_is_quote (word))
    {
      wisp_word_t quoted = wisp_car (wisp_cdr (word));
      fprintf (f, "'");
      wisp_dump (f, quoted);
    }

  else if (WISP_IS_LIST_PTR (word))
    {
      fprintf (f, "(");

      while (word != NIL)
        {
          wisp_word_t *cons = wisp_deref (word);
          wisp_word_t  car = cons[0];
          wisp_word_t  cdr = cons[1];

          wisp_dump (f, car);

          if (cdr == NIL)
            {
              break;
            }
          else if (WISP_IS_LIST_PTR (cdr))
            {
              fprintf (f, " ");
              word = cdr;
            }
          else
            {
              fprintf (f, " . ");
              wisp_dump (f, cdr);
              break;
            }
        }

      fprintf (f, ")");
    }

  else if (WISP_IS_STRUCT_PTR (word))
    {
      fprintf (f, "«");

      wisp_word_t *struct_header = wisp_deref (word);

      wisp_dump (f, struct_header[1]);
      fprintf (f, " 0x%X»", word & ~WISP_LOWTAG_MASK);
    }

  else if (WISP_IS_OTHER_PTR (word))
    {
      wisp_word_t *header = wisp_deref (word);
      if (header[0] == WISP_SYMBOL_HEADER)
        {
          wisp_word_t *string_header = wisp_deref (header[4]);
          fprintf (f, "%s", wisp_string_buffer (string_header));
        }
      else
        wisp_not_implemented ();
    }

  else
    {
      fprintf (f, "{%d}\n", word);
    }
}
