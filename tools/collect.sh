#!/bin/bash

function simpletest() {
    dir="$1"
    all=0
    count=0
    gold=0
    error=0
    
    if [ "x$dir" = "x" ]; then
        dir="./logs/"
    fi
    
    for i in `ls $dir/*.log`; do
        if [ "`tail -n 3 $i | head -n 1`" = "empty move!" ]; then
            error=`expr $error + 1`
            continue
        fi
    
        result=`tail -n 2 $i | head -n 1 | sed -e "s/\w* won //"`
        all=`expr $all + $result`
        count=`expr $count + 1`
        if [ $result -gt 0 ]; then
            gold=`expr $gold + 1`
        fi
    done
    
    echo "average score: `expr $all / $count`"
    echo "gold won     :  $gold/$count"
    echo "errors count :  $error"
}

function aei() {
    echo "Not implemented yet."
}

case $1 in
    (simpletest) simpletest $2;;
    (aei) aei $2;;
    (*) simpletest $1;;
esac
