
#include <stdio.h>
#include <getopt.h>

const struct option my_options[] = {{0,0,0,0}};
const char *my_optstring = "";

int sequence_length = 0;
int comment_length = 0;
static const int comment_max_length = 15;

void
process_option(int code, char *optarg)
{
  /* pass */
}

void
post_process(void)
{
}

void
init_sequence(void)
{
  sequence_length = 0;
}

void
handle_sequence(char c)
{
  if ((c == 'A') || (c == 'T') || (c == 'G') || (c == 'C')) sequence_length++;
}

void
terminate_sequence()
{
  printf("%d\n", sequence_length);
}

void
init_comment()
{
  comment_length = 0;
  printf("> ");
}

void
handle_comment(char c)
{
  ++comment_length;
  putchar(c);
}

void
terminate_comment()
{
  for (; comment_length < comment_max_length; ++comment_length)
    putchar(' ');
  putchar(' ');
}
