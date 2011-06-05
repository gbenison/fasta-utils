/*
 * GCB 5jun11
 *
 * Echo FASTA input sequences, including only 'n' first
 * bases.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <getopt.h>
#include "parameter.h"

const char *my_optstring = "";
const struct option my_options[] = {{0,0,0,0}};
int cutoff = 117; /* default length */

int idx = 0;

/* First non-option argument is interpreted as the cutoff length. */
void process_args(int optind, int argc, char *argv[])
{
  if ((argc - optind) > 0)
    {
      char *end;
      cutoff = strtol(argv[optind], &end, 0);
      assert(*end == '\0');
    }
}

void process_option(int code, char *optarg) { /* pass */ }
void post_process() { /* pass */ }

void init_sequence() {idx = 0;}
void handle_sequence(char c)
{
  ++idx;
  if(idx <= cutoff)
    {
      putchar(c);
      if ((idx % COLUMN_WIDTH) == 0) putchar('\n');
    }
}

void terminate_sequence() { putchar('\n'); }

/* comments are echoed */
void init_comment() { printf("> "); }
void handle_comment(char c) { putchar(c); }
void terminate_comment() { putchar('\n'); }
