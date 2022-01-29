#include "wisp.h"

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

  return wisp_intern_symbol (name, WISP);
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
