#!/bin/bash

function changedirectory() {
    dir="$1"
    if [ "x$dir" = "x" ]; then
        dir="./logs/"
    fi
    echo $dir
}

function Fight() {
    cd $1
    all=0
    count=0
    gold=0
    error=0

    for i in `ls *.log`; do
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
    awk -f `dirname $0`/collect.awk $1/*.log
}

case $1 in
    (Fight) Fight `changedirectory $2`;;
    (aei)   aei   `changedirectory $2`;;
    (*)     Fight `changedirectory $1`;;
esac
