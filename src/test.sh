#!/bin/sh

make && ./tracer && convert out.tga out.png && eog out.png
