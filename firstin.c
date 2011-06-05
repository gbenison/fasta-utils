
#include <stdio.h>

extern FILE *yyin;

void
process_args(int optind, int argc, char *argv[])
{
  yyin = (optind < argc) ? fopen(argv[optind], "r") : stdin;
}
