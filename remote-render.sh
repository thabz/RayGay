#!/bin/sh

REMOTE_HOST=$1
SCENE_FILE=$2
REMOTE_DIR=remote-tracer
REMOTE_TGA=remote.tga
REMOTE_PNG=remote.png

echo $REMOTE_HOST

echo Copying binary and scene files to $REMOTE_HOST
strip src/tracer
ssh $REMOTE_HOST mkdir $REMOTE_DIR
scp -C src/tracer $REMOTE_HOST:$REMOTE_DIR
rsync -avz --delete scenes $REMOTE_HOST:$REMOTE_DIR
ssh $REMOTE_HOST ./$REMOTE_DIR/tracer $REMOTE_DIR/scenes/`basename $SCENE_FILE` $REMOTE_DIR/$REMOTE_TGA
scp -C $REMOTE_HOST:$REMOTE_DIR/$REMOTE_TGA .
convert $REMOTE_TGA $REMOTE_PNG
rm $REMOTE_TGA
eog $REMOTE_PNG
