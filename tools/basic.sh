#!/bin/bash
cd `dirname $0`/..

LOGs_DIR="logs/"
mkdir -p $LOGs_DIR

function logfile() {
    echo "$LOGs_DIR`hostname`$1.`date '+%F_%T'`.log"
}
