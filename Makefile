NAME = rabbocop

SRC_Hs = BitRepresenation.hs MyBits.hs BitEval.hs MTDf.hs Hash.hs
LINK_C = clib.c hash.c eval.c
LINK_H = clib.h

LINK_O = ${LINK_C:.c=.o}
SRC = ${SRC_Hs} ${LINK_C} ${LINK_H}

HC = ghc
HFLAGS = -O2 -Wall -fexcess-precision -fdicts-cheap -threaded # -fhpc -prof -auto-all # -funbox-strict-fields
CC = gcc
CFLAGS = -O2 -std=c99 -Wall -pedantic

all: Main

Main: Main.hs ${SRC} ${LINK_O}
	${HC} --make Main.hs ${LINK_O} ${HFLAGS}

Test: Test.hs ${SRC} ${LINK_O}
	${HC} --make Test.hs ${LINK_O} ${HFLAGS}

runtest: Test
	./Test # ${RUN_PARAMS}

${LINK_O}: ${LINK_C} ${LINK_H}

clean:
	@echo Cleaning
	rm -f *.o *.hi *.prof

dist:
	rm ${NAME}.tar.bz2
	tar cjvf ${NAME}.tar.bz2 *.hs *.c *.h .vimrc .ghci Makefile .git .gitignore

.PHONY: all clean dist runtest
