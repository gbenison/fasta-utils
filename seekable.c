/*
 * Implement 'seekable' sequence data on top of stream-oriented lower layer -
 * allow client programs to look at any part of the sequence, not just
 * in its natural order
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

#include <stdlib.h>

static char *sequence;
static char *sequence_ptr;
int sequence_max_length;

static char *comment;
static char *comment_ptr;
int comment_max_length;

#define INIT_ARRAY(name, init_length) {\
  name##_max_length = init_length; \
  name = (char*)malloc(name##_max_length); \
  name##_ptr = name; \
}

#define PUSH_ARRAY(name, c) {\
  if ((name##_ptr - name) >= (name##_max_length - 1))  	      \
    {name##_max_length *= 2; \
     int len = name##_ptr - name; \
     name = realloc(name, name##_max_length); name##_ptr = name + len;} \
  *name##_ptr = c; ++name##_ptr; }

extern void process_sequence(char*, char*);

void init_sequence()         { INIT_ARRAY(sequence, 128); }
void handle_sequence(char c) { PUSH_ARRAY(sequence, c); }
void terminate_sequence()    { *sequence_ptr = 0;
                               process_sequence(comment, sequence); }

void init_comment()          { INIT_ARRAY(comment, 32); }
void handle_comment(char c)  { PUSH_ARRAY(comment, c);  }
void terminate_comment()     { *comment_ptr = 0; }
