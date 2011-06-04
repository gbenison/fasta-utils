
CFLAGS=-g `pkg-config --cflags glib-2.0`
LDFLAGS=`pkg-config --libs glib-2.0`

all: fastacount fastagap fastatail fastahead fastalint fastacomplement fastastack fastalength

fastalength: fastalength.yy.o
	gcc -o$@ $(LDFLAGS) $^ -lfl

fastacomplement: fastacomplement.yy.o
	gcc -o$@ $(LDFLAGS) $^ -lfl

fastastack: fastastack.yy.o
	gcc -o$@ $(LDFLAGS) $^ -lfl

fastalint: fastalint.yy.o
	gcc -o$@ $< -lfl

fastatail: fastatail.yy.c
	gcc -o$@ $< -lfl

fastahead: fastahead.yy.c
	gcc -o$@ $< -lfl

fastacount: lexer.yy.o fastacount.o firstin.o
	gcc -o$@ $^ -lfl

fastagap: fastagap.yy.c
	gcc -o$@ $< -lfl

%.yy.c:%.fl
	flex -o$@ $<

