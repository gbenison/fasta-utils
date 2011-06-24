
CFLAGS=-g `pkg-config --cflags glib-2.0`
LDFLAGS=`pkg-config --libs glib-2.0`

all: fastacount fastagap fastatail fastahead fastalint \
     fastacomplement fastastack fastacat fastaorf

fastaorf: fastaorf.hs
	ghc -o $@ $^

fastacomplement: lexer.yy.o seekable.o fastacomplement.o firstin.o
	gcc -o$@ $^ -lfl

fastastack: fastastack.o lexer.yy.o seekable.o firstin.o
	gcc -o$@ $(LDFLAGS) $^ -lfl

fastalint: fastalint.o firstin.o lexer.yy.o
	gcc -o$@ $^ -lfl

fastatail: fastatail.o lexer.yy.o
	gcc -o$@ $^ -lfl

fastahead: fastahead.o lexer.yy.o
	gcc -o$@ $^ -lfl

fastacount: lexer.yy.o fastacount.o firstin.o
	gcc -o$@ $^ -lfl

fastagap: lexer.yy.o fastagap.o firstin.o
	gcc -o$@ $^ -lfl

fastacat: lexer.yy.o fastacat.o firstin.o
	gcc -o$@ $^ -lfl

%.yy.c:%.fl
	flex -o$@ $<

