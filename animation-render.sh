#!/bin/sh

# Testing arguments num
if [ $# -lt 2 ]
then
    echo "Usage: animation-render.sh [OPTION...] SCENEFILE FRAMES"
    echo "       -r                   Resume rendering"
    echo "       -f                   Fast preview rendering"
    echo 
    echo "The global variables frame and clock are set before the"
    echo "scene is rendered."
    exit 1
fi

# Parse options
FINITO="no"
RESUME="no"
FAST_PREVIEW_SCHEME=""

while [ $FINITO = "no" ]
do        
   case $1 in
      "-r") RESUME="yes"
            echo "Resume rendering..."
            shift;;
      "-f") FAST_PREVIEW_SCHEME="(set-settings '(fast-preview #t))"
            echo "Fast preview rendering rendering enabled."
            shift;;            
      *)    FINITO="yes";;
   esac   
done        

FRAMES_NUM=$2
SCENE_FILE=$1

# Testing that scene-file exists
if [ ! -f $SCENE_FILE ]
then
    echo "Error: File '$SCENE_FILE' not found."
    exit 1
fi

if [ $FRAMES_NUM -lt 1 ]
then 
   echo "Error: Number of frames must be > 0"
   exit 1
fi   

i=0
while [ $i -lt $FRAMES_NUM ]
do
   OUTPUT_FILE=output`printf %05d $i`.png
   if [ $RESUME = "yes" -a -f $OUTPUT_FILE ]
   then
        echo "Skipping frame $i. $OUTPUT_FILE already exists."
   else            
        echo "Rendering $SCENE_FILE frame $i of $FRAMES_NUM to $OUTPUT_FILE"
        EXPR="(define frame $i) (define clock (/ $i $FRAMES_NUM)) $FAST_PREVIEW_SCHEME"
        echo $EXPR
        ./src/raygay -b -e "$EXPR" $SCENE_FILE $OUTPUT_FILE
        if [ $? -ne 0 ]; then
          exit $?
        fi
   fi
   i=`expr $i + 1`
done

exit 0
