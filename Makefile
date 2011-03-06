
CFLAGS=-g

all: fastacount fastagap fastatail fastahead fastalint fastacomplement

fastacomplement: fastacomplement.yy.o buffer.o
	gcc -o$@ $^ -lfl

fastalint: fastalint.yy.o
	gcc -o$@ $< -lfl

fastatail: fastatail.yy.c
	gcc -o$@ $< -lfl

fastahead: fastahead.yy.c
	gcc -o$@ $< -lfl

fastacount: fastacount.yy.c
	gcc -o$@ $< -lfl

fastagap: fastagap.yy.c
	gcc -o$@ $< -lfl

%.yy.c:%.fl
	flex -o$@ $<

