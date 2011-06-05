/*
 * GCB 4jun11
 *
 * Report length of FASTA sequences, excluding gaps.
 * normal mode - report only the length of the first sequence in the input.
 * verbose mode - report length of all sequences and echo comments
 */

#include <stdio.h>
#include <getopt.h>

int verbose_mode = 0;

const char *my_optstring = "v";
const struct option my_options[] = {
  {"verbose", no_argument, NULL, 'v'},
  {0,0,0,0}
};

int sequence_idx = 0;
int sequence_length = 0;
int comment_length = 0;
static const int comment_max_length = 15;

void
process_option(int code, char *optarg)
{
  if (code == 'v') verbose_mode = 1;
}

void post_process(void)  { /* pass */ }
void init_sequence(void) { sequence_length = 0; ++sequence_idx; }

void
handle_sequence(char c)
{
  if ((c == 'A') || (c == 'T') || (c == 'G') || (c == 'C')) sequence_length++;
}

void terminate_sequence()
{
  if (verbose_mode)
    printf("%d\n", sequence_length);
  else if (sequence_idx == 1) printf("%d", sequence_length);
}

void init_comment() { comment_length = 0; if (verbose_mode) printf("> "); }
void handle_comment(char c) { ++comment_length; if (verbose_mode) putchar(c); }

void
terminate_comment()
{
  if (verbose_mode)
    {
      for (; comment_length < comment_max_length; ++comment_length)
	putchar(' ');
      putchar(' ');
    }
}
