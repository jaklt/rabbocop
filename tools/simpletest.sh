#!/bin/bash
source `dirname $0`/basic.sh

INFO_FILE=$LOGs_DIR`hostname`.$$.simpletest
mkdir -p $LOGs_DIR
touch $INFO_FILE

# LOG_FILE=`logfile`
# cat data/aei-roundrobin.cfg > $LOG_FILE.running
# make play > $LOG_FILE 2>&1
# mv $LOG_FILE.running $LOG_FILE.done

for i in `seq 1 1`; do
    LOG_FILE="`logfile .simple`"
    echo "Writing to $LOG_FILE"
	./Fight IterativeAB MCTS > $LOG_FILE 2>&1
	# ./Fight MCTS MCTS nxtime  3 > $LOG_FILE 2>&1
    sleep 1
done

rm -f $INFO_FILE
