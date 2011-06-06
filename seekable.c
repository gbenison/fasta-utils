/*
 * GCB 5jun11
 *
 * Implement 'seekable' sequence data on top of stream-oriented lower layer -
 * allow client programs to look at any part of the sequence, not just
 * in its natural order
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
    {name##_max_length *= 2; name = realloc(name, name##_max_length); } \
  *name##_ptr = c; ++name##_ptr; }

extern void process_sequence(char*, char*);

void init_sequence()         { INIT_ARRAY(sequence, 128); }
void handle_sequence(char c) { PUSH_ARRAY(sequence, c); }
void terminate_sequence()    { *sequence_ptr = 0;
                               process_sequence(comment, sequence); }

void init_comment()          { INIT_ARRAY(comment, 32); }
void handle_comment(char c)  { PUSH_ARRAY(comment, c);  }
void terminate_comment()     { *comment_ptr = 0; }
