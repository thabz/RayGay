
INCLUDES = -I$(top_srcdir)/src 

LDADD = $(top_srcdir)/src/libtracer.a \
	    $(top_srcdir)/src/materials/libmaterials.a \
	    $(top_srcdir)/src/paths/libpaths.a \
	    $(top_srcdir)/src/lights/liblights.a \
	    $(top_srcdir)/src/image/libimage.a \
	    $(top_srcdir)/src/math/libmath.a 

noinst_PROGRAMS = testmath testpaths testspace testobjects testspeed testlanguage teststats #testthreads

testpaths_SOURCES = testpaths.cpp

testmath_SOURCES = testmath.cpp

testobjects_SOURCES = testobjects.cpp

testspace_SOURCES = testspace.cpp

testspeed_SOURCES = testspeed.cpp

testlanguage_SOURCES = testlanguage.cpp

teststats_SOURCES = teststats.cpp

#testthreads_SOURCES = testthreads.cpp

TESTS = teststats testlanguage testmath testpaths testspace testobjects #testthreads

