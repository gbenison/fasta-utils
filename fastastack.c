/*
 * Stack multiple FASTA input sequences for easy comparison.
 *
 * Copyright (C) 2011, Greg Benison
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
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
