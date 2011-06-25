/*
 * Convert each sequence in FASTA input to its reverse complement.
 *
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
