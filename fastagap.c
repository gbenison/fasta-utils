/*
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
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

int verbose_mode = 0;
int sequence_idx = 0;

int exon_start = 0;
int exon_mode = 0;
int idx = 0;
int first_exon_idx = 0;

static const int exon_min_length = 8;

const char *my_optstring = "v";
const struct option my_options[] = {
  {"verbose", no_argument, NULL, 'v'},
  {0,0,0,0}
};

static void
report_exon(int idx)
{
  exon_mode = 0;
  if ((idx - exon_start + 1) >= exon_min_length)
    printf("  %d -- %d\n", exon_start, idx);
}

static void
start_exon()
{
  exon_mode = 1;
  exon_start = idx;
}

void process_option(int code, char *optarg) {
  if (code == 'v') verbose_mode = 1;
}

void post_process() {}

void init_sequence() { exon_mode = 0; idx = 0; sequence_idx++; }

void
handle_sequence(char c)
{
  ++idx;
  if (verbose_mode)
    {
      if ((c == '-') && exon_mode)
	report_exon(idx - 1);
      else if (!exon_mode && (c != '-')) start_exon();
    }
    else
    {
      if ((c != '-') && (first_exon_idx == 0))
	first_exon_idx = idx;
    }
}

void
terminate_sequence()
{
  if (verbose_mode)
    {
      if (exon_mode) report_exon(idx);
    }
  else if (sequence_idx == 1)
    printf ("%d", first_exon_idx);
}

void init_comment() { if (verbose_mode) printf("> "); }
void handle_comment(char c) { if (verbose_mode) putchar(c); }
void terminate_comment() { if (verbose_mode) putchar('\n'); }
