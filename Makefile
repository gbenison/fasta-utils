
all: fastacount fastagap fastatail fastahead fastalint

fastalint: fastalint.yy.c
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

