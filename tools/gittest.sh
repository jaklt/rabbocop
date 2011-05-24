#!/bin/bash
source `dirname $0`/basic.sh
LOG_FILE=`logfile .gittest`

A_FILE="A-`hostname`-`echo $$`"
B_FILE="B-`hostname`-`echo $$`"

config=`cat <<END
[global]
rounds       = 10
# rounds       = 1
loglevel     = DEBUG
write_pgn    = False
pgn_filename = result.pgn
# timecontrol  = 30s/60s/100/60s/20m
timecontrol  = 3s/10s/100/60s/10m
bots         = A B
bot_hash     = 200

[A]
communication_method = stdio
cmdline = ../$A_FILE +RTS -A50m

[B]
communication_method = stdio
cmdline = ../$B_FILE +RTS -A50m
END`

machineNum=`hostname | cut -c 5,6`
commit="`git log -n $(expr $machineNum - 2) --pretty=format:'%h' | tail -n 1`"

A=$commit
A_BOT="IterativeAB"
B=$commit
B_BOT="IterativeAB"

# Make bots
git checkout $A
make $A_BOT
mv $A_BOT $A_FILE
git checkout master

git checkout $B
make $B_BOT
mv $B_BOT $B_FILE
git checkout master

# Start fight
echo "$config" > aei-1.1/roundrobin.cfg
echo "Log will be written to $LOG_FILE."
cd aei-1.1
python roundrobin.py > ../$LOG_FILE 2>&1
cd ..

echo >> $LOG_FILE
echo A: $A_BOT $A >> $LOG_FILE
echo B: $B_BOT $B >> $LOG_FILE
rm -f $A_FILE $B_FILE
