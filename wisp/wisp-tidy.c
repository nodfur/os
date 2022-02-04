#include "wisp.h"

int
wisp_instance_size (wisp_word_t *data)
{
  wisp_word_t length = *data >> 8;
  return wisp_align (WISP_WORD_SIZE * (length + 1));
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
      wisp_not_implemented ();
    }

}

wisp_word_t
wisp_copy (wisp_word_t ptr)
{
  wisp_word_t *data = wisp_deref (ptr);
  wisp_word_t lowtag = ptr & 7;

  int n = wisp_object_size (data);
  int dst = pile_free;

  pile_free += n;
  memcpy (heap + dst, data, n);

  return dst | lowtag;
}

wisp_word_t
wisp_move (wisp_word_t x)
{
  if (!WISP_IS_PTR (x))
    return x;

  wisp_word_t *header = wisp_deref (x);

  if (!(x >= room && x <= (room + heap_size)))
    return x;

  if (!(header[0] >= pile && header[0] <= (pile + heap_size)))
    header[0] = wisp_copy (x);

  return header[0];
}

void
wisp_scavenge (void)
{
  wisp_word_t *header = wisp_deref (pile_scan);
  pile_scan += WISP_WORD_SIZE;

  switch (*header & 0xff)
    {
    case WISP_WIDETAG_INSTANCE:
    case WISP_WIDETAG_SYMBOL:
      {
        int n = *header >> 8;

        for (int i = 0; i < n; i++)
          {
            *header = wisp_move (pile_scan);
            pile_scan += WISP_WORD_SIZE;
          }

        break;
      }

    case WISP_WIDETAG_STRING:
      {
        pile_scan += wisp_string_size (header);
        break;
      }

    default:
      wisp_crash ("not implemented");
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
  for (int i = 0; i < wisp_cache_size; i++)
    wisp_cache[i] = wisp_move (wisp_cache[i]);

  while (pile_scan < pile_free)
    wisp_scavenge ();

  wisp_flip ();
}
