
SUBDIRS = normalperturbers

INCLUDES = -I$(top_srcdir)/src 

noinst_LTLIBRARIES = libmaterials.la

libmaterials_la_SOURCES = material.cpp perlin.cpp plastic.cpp wood.cpp checker.cpp marble.cpp
libmaterials_la_LIBADD = normalperturbers/libnormalperturbers.la

EXTRA_DIST = material.h materials.h perlin.h plastic.h wood.h checker.h marble.h
