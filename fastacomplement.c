/*
 * GCB 5jun11
 *
 * Convert each sequence in FASTA input to its reverse complement.
 *
 * designed to work with seekable.o
 */

#include <getopt.h>
#include <stdio.h>
#include "parameter.h"

const char *my_optstring = "";
const struct option my_options[] = {{0,0,0,0}};

/* FIXME doesn't handle lower-case well */
static char
complement(char c)
{
  if      (c == 'G') return 'C';
  else if (c == 'C') return 'G';
  else if (c == 'A') return 'T';
  else if (c == 'T') return 'A';
  else return c;
}

static void
reverse_string(char *str)
{
  char *head = str;
  while (*head != 0) ++head;
  --head;
  while (str < head) {
    char tmp = *str;
    *str = *head;
    *head = tmp;
    ++str;
    --head;
  }
}

void process_option(int code, char *optarg) { /* pass */ }
void post_process() { /* pass */ }

void
process_sequence(char *comment, char *sequence)
{
  reverse_string(sequence);
  printf(">%s", comment);

  char *head = sequence;

  while (*head != 0) {
    if (((head - sequence) % COLUMN_WIDTH) == 0) putchar('\n');
    putchar(complement(*head)); ++head;
  }
}
