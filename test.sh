#!/bin/sh

echo $ARGV

make && ./src/tracer scenes/staircase.gay out.tga && convert out.tga out.png && eog out.png
