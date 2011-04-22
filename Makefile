NAME = rabbocop
BINs = IterativeAB MCTS MTDf Test

SRC_Hs = \
	AEI.hs \
	AlphaBeta.hs \
	BitEval.hs \
	BitRepresentation.hs \
	Hash.hs \
	HaskellHash.hs \
	Helpers.hs \
	IterativeAB.hs \
	JudyHash.hs \
	MCTS.hs \
	MonteCarloEval.hs \
	MTDf.hs \
	MyBits.hs \

LINK_C = clib.c eval.c
LINK_H = clib.h
STATIC_EVAL_TABLES = data/staticeval_g.c data/staticeval_s.c

LINK_O = ${LINK_C:.c=.o}
SRC = ${SRC_Hs} ${LINK_C} ${LINK_H}

HC = ghc
HFLAGS = -O2 -Wall -fexcess-precision -fdicts-cheap -threaded
#        -fhpc -funbox-strict-fields
CC = gcc
CFLAGS = -O2 -std=c99 -Wall -pedantic

ifdef PROF
	HFLAGS += -prof -fforce-recomp -auto-all
endif

ifdef JUDY
	HFLAGS += -DJUDY
endif


all: IterativeAB MCTS MTDf

IterativeAB MCTS MTDf: HFLAGS += -DENGINE

$(BINs): % : %.hs ${SRC} ${LINK_O}
	${HC} --make $@.hs ${LINK_O} ${HFLAGS}

runtest: Test
	time -p ./Test # ${RUN_PARAMS}

${LINK_O}: ${LINK_C} ${LINK_H}

eval.o: ${STATIC_EVAL_TABLES}

# Prepare part of static evaluation based on considering actual position
tools/BoardToCode: tools/BoardToCode.hs
	${HC} --make tools/BoardToCode.hs ${HFLAGS}

${STATIC_EVAL_TABLES}: data/staticeval.txt tools/BoardToCode
	./tools/BoardToCode data/staticeval.txt         > data/staticeval_g.c
	./tools/BoardToCode data/staticeval.txt REVERSE > data/staticeval_s.c

clean:
	@echo Cleaning
	rm -f *.o *.hi *.prof *.tix
	rm -f tools/*.o tools/*.hi
	rm -f data/staticeval_g.c
	rm -f data/staticeval_s.c

# Download and instal GUI
play: IterativeAB aei-1.1/roundrobin.py arimaa-client/gui.py aei-1.1/roundrobin.cfg
	cd aei-1.1; python roundrobin.py

aei-1.1/roundrobin.py:
	wget http://arimaa.janzert.com/aei/aei-1.1.zip --directory-prefix=aei-1.1
	unzip aei-1.1/aei-1.1.zip

aei-1.1/roundrobin.cfg: data/aei-roundrobin.cfg
	cp data/aei-roundrobin.cfg aei-1.1/roundrobin.cfg

arimaa-client/gui.py:
	bzr branch lp:arimaa-client

.PHONY: all clean runtest play
