#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

echo "Now running aclocal"
aclocal 

echo "Now running automake"
automake --add-missing 

echo "Now running autoconf"
autoconf

echo "Now runnning $srcdir/configure"
$srcdir/configure 


echo "'CPPFLAGS=-pg ./configure' if you want profiling enabled. The tmon.out can be analyzed with kprof or gprof."
