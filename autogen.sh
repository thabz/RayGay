#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

echo "Now running aclocal"
aclocal 

echo "Now running libtoolize"
libtoolize --force


echo "Now running automake"
automake --add-missing 

echo "Now running autoconf"
autoconf

echo "Now run ./configure followed by make. "
#$srcdir/configure 

