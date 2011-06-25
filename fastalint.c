/*
 * Reformat input stream to a standard column width;
 * optionally add a comment to the first sequence
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
