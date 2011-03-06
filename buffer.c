#include <stdlib.h>

int fb_count = 0;
static int fb_size = 0;
static char *buffer = NULL;

void
fb_clear()
{
  fb_count = 0;
}

void
fb_push(char c)
{
  if (fb_count >= fb_size)
    {
      fb_size = (fb_size == 0) ? 512 : fb_size * 2;
      buffer = realloc(buffer, fb_size);
    }
  buffer[fb_count] = c;
  ++fb_count;
}

char
fb_peek(int idx)
{
  return (idx < fb_count) ? buffer[idx] : 'X';
}

