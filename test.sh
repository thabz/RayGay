#!/bin/sh

echo $ARGV

make && ./src/tracer scenes/testscene.gay out.tga && convert out.tga out.png && eog out.png
