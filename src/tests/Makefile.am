
INCLUDES = -I$(top_srcdir)/src 

all_ldadd = $(top_srcdir)/src/libtracer.a \
	    $(top_srcdir)/src/materials/libmaterials.a \
	    $(top_srcdir)/src/paths/libpaths.a \
	    $(top_srcdir)/src/lights/liblights.a \
	    $(top_srcdir)/src/image/libimage.a \
	    $(top_srcdir)/src/math/libmath.a 

noinst_PROGRAMS = testmath testpaths testspace testobjects

testpaths_SOURCES = testpaths.cpp
testpaths_LDADD = $(all_ldadd)
testpaths_DEPENDENCIES = $(all_ldadd)

testmath_SOURCES = testmath.cpp
testmath_LDADD = $(all_ldadd)

testobjects_SOURCES = testobjects.cpp
testobjects_LDADD = $(all_ldadd)

testspace_SOURCES = testspace.cpp
testspace_LDADD = $(all_ldadd)

TESTS = testmath testpaths testspace testobjects

