#include "wisp.h"

int
wisp_instance_size (wisp_word_t *data)
{
  wisp_word_t length = *data >> 8;
  /* WISP_DEBUG ("instance length %d\n", length); */
  return wisp_align ((1 + length) * WISP_WORD_SIZE);
}

int
wisp_string_size (wisp_word_t *data)
{
  wisp_word_t length = *data >> 8;
  return wisp_align (WISP_WORD_SIZE + length + 1);
}

int
wisp_object_size (wisp_word_t *data)
{
  switch (data[0] & 0xff)
    {
    case WISP_WIDETAG_INSTANCE:
    case WISP_WIDETAG_SYMBOL:
      {
        return wisp_instance_size (data);
      }

    case WISP_WIDETAG_STRING:
      {
        return wisp_string_size (data);
      }

    default:
      WISP_DEBUG ("probably a list\n");
      return 2 * WISP_WORD_SIZE;
    }

}

wisp_word_t
wisp_copy (wisp_word_t ptr)
{
  if (!WISP_IS_PTR (ptr))
    {
      WISP_DEBUG ("leaving 0x%x ", ptr);
      wisp_dump (stderr, ptr);
      WISP_DEBUG ("\n");
      return ptr;
    }

  WISP_DEBUG ("copying ");
  wisp_dump (stderr, ptr);
  WISP_DEBUG ("\n");

  wisp_word_t *data = wisp_deref (ptr);
  wisp_word_t lowtag = ptr & 7;

  if (data[0] >= pile && data[0] <= (pile + heap_size))
    {
      WISP_DEBUG ("   already copied [0x%x]\n", data[0] & ~7);
      return data[0];
    }

  int n = wisp_object_size (data);
  int dst = pile_free;

  pile_free += n;
  WISP_DEBUG ("   [0x%x] to [0x%x]\n", dst, dst + n);

  memcpy (heap + dst, data, n);

  data[0] = dst | lowtag;

  return dst | lowtag;
}

void
wisp_scavenge (void)
{
  wisp_word_t *header = wisp_deref (pile_scan);

  WISP_DEBUG ("\nscavenging [0x%x] ", pile_scan);
  wisp_dump (stderr, *header);
  WISP_DEBUG ("\n");

  if (*header == 0)
    {
      pile_scan += WISP_WORD_SIZE;
      return;
    }

  switch (*header & 0xff)
    {
    case WISP_WIDETAG_INSTANCE:
    case WISP_WIDETAG_SYMBOL:
      {
        int n = *header >> 8;

        for (int i = 0; i < n + 1; i++)
          {
            WISP_DEBUG ("#%d ", i);
            header[i] = wisp_copy (header[i]);
          }

        pile_scan += wisp_instance_size (header);

        break;
      }

    case WISP_WIDETAG_STRING:
      {
        pile_scan += wisp_string_size (header);
        break;
      }

    default:
      {
        int n = 2;

        for (int i = 0; i < n; i++)
          {
            WISP_DEBUG ("@%d ", i);
            header[i] = wisp_copy (header[i]);
            pile_scan += WISP_WORD_SIZE;
          }

        break;
      }
    }
}

void
wisp_flip (void)
{
  int new_pile = room;
  int new_room = pile;
  int new_heap_used = pile_free;

  pile = new_pile;
  room = new_room;
  heap_used = new_heap_used;

  pile_free = 0;
  pile_scan = 0;
}

void
wisp_tidy (void)
{
  /* for (int i = 0; i < wisp_cache_size; i++) */
  /*   { */
  /*     WISP_DEBUG ("cache %d\n", i); */
  /*     wisp_cache[i] = wisp_move (wisp_cache[i]); */
  /*   } */

  WISP_DEBUG ("\n* collecting garbage\n");

  WISP_CACHE (WISP) = wisp_copy (WISP_CACHE (WISP));

  while (pile_scan < pile_free)
    wisp_scavenge ();

  wisp_flip ();
}
