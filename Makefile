NAME = rabbocop

SRC_Hs = BitRepresenation.hs MyBits.hs BitEval.hs MTDf.hs Hash.hs AlphaBeta.hs MCTS.hs MonteCarloEval.hs HaskellHash.hs
LINK_C = clib.c eval.c
LINK_H = clib.h
STATIC_EVAL_TABLES = data/staticeval_g.c data/staticeval_s.c

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
	time ./Test # ${RUN_PARAMS}

${LINK_O}: ${LINK_C} ${LINK_H}

eval.o: ${STATIC_EVAL_TABLES}

${STATIC_EVAL_TABLES}: data/staticeval.txt tools/BoardToCode.hs
	${HC} --make tools/BoardToCode.hs ${HFLAGS}
	./tools/BoardToCode data/staticeval.txt         > data/staticeval_g.c
	./tools/BoardToCode data/staticeval.txt REVERSE > data/staticeval_s.c

clean:
	@echo Cleaning
	rm -f *.o *.hi *.prof
	rm -f tools/*.o tools/*.hi
	rm -f data/staticeval.c

dist:
	rm ${NAME}.tar.bz2
	tar cjvf ${NAME}.tar.bz2 *.hs *.c *.h .vimrc .ghci Makefile .git .gitignore

play: Main aei-1.1/roundrobin.py arimaa-client/gui.py aei-1.1/roundrobin.cfg
	cd aei-1.1; python roundrobin.py

aei-1.1/roundrobin.py:
	wget http://arimaa.janzert.com/aei/aei-1.1.zip --directory-prefix=aei-1.1
	unzip aei-1.1/aei-1.1.zip

aei-1.1/roundrobin.cfg: data/aei-roundrobin.cfg
	cp data/aei-roundrobin.cfg aei-1.1/roundrobin.cfg

arimaa-client/gui.py:
	bzr branch lp:arimaa-client

.PHONY: all clean dist runtest play
