
AM_CPPFLAGS = -I$(top_srcdir)/src 

noinst_LTLIBRARIES = libmath.la

libmath_la_SOURCES = vector.cpp matrix.cpp vector2.cpp functions.cpp halton.cpp  rootfinder.cpp polynomial.cpp sturmsequence.cpp matrix3.cpp mersennetwister.cpp poisson_disc.cpp interval.cpp quaternion.cpp physics.cpp contour.cpp

EXTRA_DIST = vector.h 	\
             vector2.h 	\
	     functions.h \
	     function.h	\
	     halton.h	\
             matrix.h 	\
	     matrix3.h	\
             polynomial.h 	\
	     quaternion.h	\
	     rootfinder.h	\
	     sturmsequence.h	\
             qmcsequence.h 	\
	     rng.h		\
	     mersennetwister.h	\
	     poisson_disc.h	\
	     interval.h		\
	     contour.h		\
	     physics.h		\
	     constants.h

# -----------------------------------------------------
# Tests
# -----------------------------------------------------
check_PROGRAMS = testmath
testmath_SOURCES = testmath.cpp
testmath_LDADD = libmath.la
TESTS = testmath

