#!/bin/sh
prefix="u-pl"
machines=`seq 3 23`
cores="1 2"
args="$@"

if [ -x "$args" ]; then
    script="./$args"
else
    script="$args"
fi

for ma in $machines; do
    for core in $cores; do
        echo "Machine $prefix$ma, starting script ($core)"
        echo "======================================"
        ssh $prefix$ma "cd `pwd`; screen -d -m $script" || break 1
        sleep 1
        echo && echo
    done
done
