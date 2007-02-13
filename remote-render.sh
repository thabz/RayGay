#!/bin/sh

REMOTE_HOST=$1
SCENE_FILE=$2
REMOTE_DIR=remote-tracer
REMOTE_BIN_DIR=bin
REMOTE_PNG=remote.png

echo $REMOTE_HOST

echo Copying binary and scene files to $REMOTE_HOST
ssh $REMOTE_HOST mkdir -p $REMOTE_DIR
rsync -avz --delete src/tracer scenes $REMOTE_HOST:$REMOTE_DIR
ssh $REMOTE_HOST ./$REMOTE_BIN_DIR/tracer $REMOTE_DIR/scenes/`basename $SCENE_FILE` $REMOTE_DIR/$REMOTE_PNG
scp -C $REMOTE_HOST:$REMOTE_DIR/$REMOTE_PNG .
