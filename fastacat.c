/*
 * GCB 6jun11
 *
 * catentate FASTA sequences (literally end-to-end) in input;
 * retain only initial comment
 */

#include <getopt.h>
#include "parameter.h"

const char *my_optstring = "";
const struct option my_options[] = {{0,0,0,0}};

static int idx = 0;

void process_option(int code, char *optarg) { /* pass */ }
void post_process(void)  { putchar('\n'); }
void init_sequence(void) { /* pass */ }

void handle_sequence(char c)
{
  ++idx;
  putchar(c);
  if ((idx % COLUMN_WIDTH) == 0) putchar('\n');
}

void terminate_sequence() { /* pass */ }

int comment_idx = 0;

void init_comment()
{
  comment_idx++;
  if (comment_idx == 1) putchar ('>');
}

void handle_comment(char c) { if (comment_idx == 1) putchar(c); }
void terminate_comment() { if (comment_idx == 1) putchar('\n'); }

