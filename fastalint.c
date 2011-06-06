/*
 * GCB 5jun11
 *
 * Reformat input stream to a standard column width;
 * optionally add a comment to the first sequence
 */

#include <stdio.h>
#include <getopt.h>
#include "parameter.h"

static int idx = 0;

const char *my_optstring = "c:";
const struct option my_options[] = {
  {"comment", required_argument, NULL, 'c'},
  {0,0,0,0}};

void process_option(int code, char *optarg) {
  if (code == 'c') { printf("> %s\n", optarg); }
}

void post_process() {}

void init_sequence() { idx = 0; }
void handle_sequence(char c) {
  ++idx;
  putchar(c);
  if ((idx % COLUMN_WIDTH) == 0) putchar('\n');
}

void terminate_sequence() { putchar('\n'); }

/* comments are echoed */
void init_comment() { printf("> "); }
void handle_comment(char c) { putchar(c); }
void terminate_comment() { putchar('\n'); }
