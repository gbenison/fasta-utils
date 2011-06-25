/*
 * catentate FASTA sequences (literally end-to-end) in input;
 * retain only initial comment
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

