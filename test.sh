#!/bin/sh

echo $ARGV

make && ./src/tracer scenes/testscene.gay out.png && eog out.png
