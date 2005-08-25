#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

echo "Now running autoheader"
autoheader

echo "Now running aclocal"
aclocal $ACLOCAL_FLAGS

if test `uname` = "Darwin"; then
   echo "Now running glibtoolize"
   glibtoolize -f -c
else 
   echo "Now running libtoolize"
   libtoolize -f -c
fi

echo "Now running automake"
automake --add-missing 

echo "Now running autoconf"
autoconf


echo "Now running configure"
$srcdir/configure $*

echo "Type make to build raygay"

