/*
 * Report length of FASTA sequences, excluding gaps.
 * normal mode - report only the length of the first sequence in the input.
 * verbose mode - report length of all sequences and echo comments
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
