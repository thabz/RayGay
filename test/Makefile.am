
SUBDIRS = gfx scenes

INCLUDES = -I$(top_srcdir)/src 

LDADD = $(top_builddir)/src/libtracer.a \
	    $(top_builddir)/src/materials/libmaterials.a \
	    $(top_builddir)/src/objects/libobjects.a \
	    $(top_builddir)/src/paths/libpaths.a \
	    $(top_builddir)/src/lights/liblights.a \
	    $(top_builddir)/src/image/libimage.a \
	    $(top_builddir)/src/space/libspace.a \
	    $(top_builddir)/src/filters/libfilters.a \
	    $(top_builddir)/src/cameras/libcameras.a \
	    $(top_builddir)/src/math/libmath.a 

check_PROGRAMS = testimage testmath testpaths testspace testobjects testspeed testlanguage teststats testthreads testparser

testparser_SOURCES = testparser.cpp
testparser_LDADD = $(top_builddir)/src/parser/libparser.a \
		   $(top_builddir)/src/materials/libmaterials.a \
		   $(top_builddir)/src/objects/libobjects.a \
		   $(top_builddir)/src/paths/libpaths.a \
		   $(top_builddir)/src/lights/liblights.a \
		   $(top_builddir)/src/space/libspace.a \
		   $(top_builddir)/src/cameras/libcameras.a \
		   $(top_builddir)/src/libtracer.a \
	           $(top_builddir)/src/filters/libfilters.a \
	    	   $(top_builddir)/src/image/libimage.a \
		   $(top_builddir)/src/math/libmath.a 

testimage_SOURCES = testimage.cpp
testimage_LDADD = $(top_builddir)/src/image/libimage.a $(top_builddir)/src/math/libmath.a

testpaths_SOURCES = testpaths.cpp

testmath_SOURCES = testmath.cpp
testmath_LDADD = $(top_builddir)/src/math/libmath.a

testobjects_SOURCES = testobjects.cpp

testspace_SOURCES = testspace.cpp
#testspace_LDADD = $(top_builddir)/src/space/libspace.a $(top_builddir)/src/libtracer.a $(top_builddir)/src/objects/libobjects.a $(top_builddir)/src/math/libmath.a  $(top_builddir)/src/materials/libmaterials.a $(top_builddir)/src/image/libimage.a

testspeed_SOURCES = testspeed.cpp
testspeed_LDADD = 

testlanguage_SOURCES = testlanguage.cpp
testlanguage_LDADD = 

teststats_SOURCES = teststats.cpp
teststats_LDADD = $(top_builddir)/src/libtracer.a

testthreads_SOURCES = testthreads.cpp
testthreads_LDADD = 

TESTS = testobjects testimage teststats testlanguage testmath testpaths testspace testthreads testparser

