
INCLUDES = -I$(top_srcdir)/src 

LDADD = $(top_srcdir)/src/libtracer.a \
	    $(top_srcdir)/src/materials/libmaterials.a \
	    $(top_srcdir)/src/paths/libpaths.a \
	    $(top_srcdir)/src/lights/liblights.a \
	    $(top_srcdir)/src/image/libimage.a \
	    $(top_srcdir)/src/math/libmath.a 

noinst_PROGRAMS = testmath testpaths testspace testobjects testspeed

testpaths_SOURCES = testpaths.cpp

testmath_SOURCES = testmath.cpp

testobjects_SOURCES = testobjects.cpp

testspace_SOURCES = testspace.cpp

testspeed_SOURCES = testspeed.cpp

TESTS = testmath testpaths testspace testobjects

