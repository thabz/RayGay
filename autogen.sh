#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

# Autoheader generates config.h.in from configure.in
echo "Now running autoheader"
autoheader

echo "Now running aclocal"
aclocal $ACLOCAL_FLAGS -I m4

# Run glibtoolize (Darwin) or libtoolize (Linux)
if [ `which glibtoolize` != "" ]; then
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

