/*
 * Echo FASTA input sequences, including all but first 'n' bases.
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
  if(idx > cutoff)
    {
      putchar(c);
      if ((idx % COLUMN_WIDTH) == 0) putchar('\n');
    }
}

/* comments are echoed */
void terminate_sequence() { putchar('\n'); }
void init_comment() { printf("> "); }
void handle_comment(char c) { putchar(c); }
void terminate_comment() { putchar('\n'); }
