
SUBDIRS = gfx scheme

INCLUDES = -I$(top_srcdir)/src 
LDADD = $(top_builddir)/src/libtracer.la

check_PROGRAMS = testimage testpaths testspace testobjects testspeed testlanguage teststats testthreads testparser

testparser_SOURCES = testparser.cpp
testimage_SOURCES = testimage.cpp
testpaths_SOURCES = testpaths.cpp
testobjects_SOURCES = testobjects.cpp
testspace_SOURCES = testspace.cpp
testspeed_SOURCES = testspeed.cpp
testlanguage_SOURCES = testlanguage.cpp
teststats_SOURCES = teststats.cpp
testthreads_SOURCES = testthreads.cpp

TESTS = testobjects testimage teststats testlanguage testpaths testspace testthreads testparser

EXTRA_DIST = testing.h

