/*
 * GCB 5jun11
 *
 * Stack multiple FASTA input sequences for easy comparison.
 */

#include <glib.h>
#include <stdio.h>
#include <getopt.h>
#include "parameter.h"

static const int comment_max_width = 24;

const char *my_optstring = "";
const struct option my_options[] = {{0,0,0,0}};

GList *comments = NULL;
GList *sequences = NULL;

void process_option(int code, char *optarg) { /* pass */ }

int n_reported = 0;

void post_process()
{
  int more = 0;
  GList *comment_ptr = comments;
  GList *sequence_ptr = sequences;

  for (; sequence_ptr != NULL; sequence_ptr = sequence_ptr->next)
    if ((*(char*)(sequence_ptr->data)) != 0) more = 1;

  if (!more) goto done;

  for (comment_ptr = comments, sequence_ptr = sequences;
       comment_ptr != NULL;
       comment_ptr = comment_ptr->next, sequence_ptr = sequence_ptr->next)
    {
      int i;
      /* report comment */
      char *comment = (char*)(comment_ptr->data);
      for (i = 0; i < comment_max_width; ++i)
	if (*comment) putchar(*comment++); else putchar('.');

      putchar(' ');
      putchar(' ');

      /* report sequence */
      for (i = 0; i < COLUMN_WIDTH; ++i)
	{
	  char *txt = (char*)(sequence_ptr->data);
	  if (*txt != 0)
	    {
	      putchar(*(char*)(sequence_ptr->data));
	      sequence_ptr->data++;
	    }
	}
      if (sequence_ptr == sequences)
	{
	  n_reported += COLUMN_WIDTH;
	  printf("  %d", n_reported);
	}
      putchar('\n');
    }
  putchar('\n');
  post_process();
 done:
  1 == 1; /* pass */
}

void process_sequence(char *comment, char *sequence)
{
  comments = g_list_append(comments, comment);
  sequences = g_list_append(sequences, sequence);
}
