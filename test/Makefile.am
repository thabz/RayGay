
INCLUDES = -I$(top_srcdir)/src 

LDADD = $(top_srcdir)/src/libtracer.a \
	    $(top_srcdir)/src/materials/libmaterials.a \
	    $(top_srcdir)/src/objects/libobjects.a \
	    $(top_srcdir)/src/paths/libpaths.a \
	    $(top_srcdir)/src/lights/liblights.a \
	    $(top_srcdir)/src/image/libimage.a \
	    $(top_srcdir)/src/space/libspace.a \
	    $(top_srcdir)/src/math/libmath.a 

noinst_PROGRAMS = testimage testmath testpaths testspace testobjects testspeed testlanguage teststats testthreads

testimage_SOURCES = testimage.cpp
testimage_LDADD = $(top_srcdir)/src/image/libimage.a $(top_srcdir)/src/math/libmath.a

testpaths_SOURCES = testpaths.cpp

testmath_SOURCES = testmath.cpp
testmath_LDADD = $(top_srcdir)/src/math/libmath.a

testobjects_SOURCES = testobjects.cpp

testspace_SOURCES = testspace.cpp
#testspace_LDADD = $(top_srcdir)/src/space/libspace.a $(top_srcdir)/src/libtracer.a $(top_srcdir)/src/objects/libobjects.a $(top_srcdir)/src/math/libmath.a  $(top_srcdir)/src/materials/libmaterials.a $(top_srcdir)/src/image/libimage.a

testspeed_SOURCES = testspeed.cpp
testspeed_LDADD = 

testlanguage_SOURCES = testlanguage.cpp
testlanguage_LDADD = 

teststats_SOURCES = teststats.cpp
teststats_LDADD = $(top_srcdir)/src/libtracer.a

testthreads_SOURCES = testthreads.cpp
testthreads_LDADD = 

TESTS = testimage teststats testlanguage testmath testpaths testspace testobjects testthreads

