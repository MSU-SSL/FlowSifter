#!/bin/bash

TIME_WINDOW=3 #s
MEMORY_ALLOCATED=1000 #kb

if [ -z "$1" ]
then
    echo "Usage: $0 pid"
    exit 66
fi

for((i=0;;++i)) { 
    vcur=`grep VmRSS /proc/$1/status | grep -o '[0-9]*'`
    v[$i]=$vcur
    past=$(($i-$TIME_WINDOW))
#    echo "$i ${v[i]} $past" 1>&2
    if (( $past > 0 ))
    then
	if (( $vcur - ${v[past]} < $MEMORY_ALLOCATED ))
	then
	    break
	fi
    fi
    sleep 1 || break
}
echo " $vcur $i"
kill $1 
