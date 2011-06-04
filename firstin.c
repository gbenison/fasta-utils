
#include <stdio.h>

extern FILE *yyin;

void
process_args(int argc, char *argv[])
{
  yyin = (argc < 1) ? stdin : fopen(argv[1], "r");
}
