#!/bin/sh

# TODO: Argument testing

i=0
while [ $i -lt $1 ]
do
   ./src/tracer $2 -f $i -F $1 -x kaj$i.png
   echo $i
   i=`expr $i + 1`
done
