#include "wisp.h"

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
      fprintf (stderr, "«");

      wisp_word_t *struct_header = wisp_deref (word);

      wisp_dump (struct_header[1]);
      fprintf (stderr, " 0x%X»", word & ~WISP_LOWTAG_MASK);
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
