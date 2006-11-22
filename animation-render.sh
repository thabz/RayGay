#!/bin/sh

# Testing arguments num
if [ $# -lt 2 ]
then
    echo "USAGE: animation-render.sh SCENEFILE FRAMES"
    exit 1
fi

FRAMES_NUM=$2
SCENE_FILE=$1

# Testing that scene-file exists
if [ ! -f $SCENE_FILE ]
then
    echo "Error: $SCENE_FILE not found."
    exit 1
fi

i=0
while [ $i -lt $FRAMES_NUM ]
do
   OUTPUT_FILE=output`printf %05d $i`.png
   echo Rendering $SCENE_FILE frame $i of $FRAMES_NUM to $OUTPUT_FILE
   ./src/tracer -b -f $i -F $FRAMES_NUM $SCENE_FILE $OUTPUT_FILE
   if [ $? -ne 0 ]; then
      exit $?
   fi
   i=`expr $i + 1`
done

exit 0

