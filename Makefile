NAME = rabbocop

SRC_Hs = BitRepresenation.hs MyBits.hs

LINK_C = clib.c
LINK_H = clib.h

LINK_O = ${LINK_C:.c=.o}
SRC = ${SRC_Hs} ${LINK_C} ${LINK_H}
OBJ = ${SRC_Hs:.hs=.hi} ${SRC_Hs:.hs=.o} Main.hi Main.o Test.hi Test.o ${LINK_O}
PARAMS = -O2 -Wall # -threaded
CC = gcc
CFLAGS = -O2 -std=c99 -Wall -pedantic

all: Main

Main: Main.hs ${SRC} ${LINK_O}
	ghc --make Main.hs ${LINK_O} ${PARAMS}

Test: Test.hs ${SRC} ${LINK_O}
	ghc --make Test.hs ${LINK_O} ${PARAMS}

runtest: Test
	./Test # ${RUN_PARAMS}

${LINK_O}: ${LINK_C} ${LINK_H}

clean:
	@echo Cleaning
	@rm -f *.o *.hi # ${OBJ}

dist:
	rm ${NAME}.tar.bz2
	tar cjvf ${NAME}.tar.bz2 *.hs *.c *.h .vimrc .ghci Makefile # .git .gitignore

.PHONY: all clean dist runtest
