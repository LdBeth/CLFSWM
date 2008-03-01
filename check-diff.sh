#!/bin/sh

DIR=../clfswm2

for i in *
do
    diff $i $DIR/$i > /dev/null
    if [ $? = 1 ]; then
	echo $i
	#cp $DIR/$i .
    fi
done
