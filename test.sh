#!/bin/sh

echo $ARGV

make && ./src/tracer scenes/benchmarkscene.gay out.png 
