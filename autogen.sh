#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

echo "Now running autoheader"
autoheader

echo "Now running aclocal"
aclocal $ACLOCAL_FLAGS

echo "Now running libtoolize"
if test `uname` = "Darwin"; then
   glibtoolize --force
else 
   libtoolize --force
fi

echo "Now running automake"
automake --add-missing 

echo "Now running autoconf"
autoconf


echo "Now run ./configure followed by make. "
$srcdir/configure 

echo "Type make to build raygay"

