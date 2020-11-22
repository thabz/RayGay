
AC_INIT(raygay, 0.5)    
AC_CONFIG_HEADERS([config.h:config.h.in])
AC_CONFIG_SRCDIR(src/main.cpp)

RAYGAY_MAJOR_VERSION=0
RAYGAY_MINOR_VERSION=7    
RAYGAY_MICRO_VERSION=0    
RAYGAY_VERSION="$RAYGAY_MAJOR_VERSION.$RAYGAY_MINOR_VERSION.$RAYGAY_MICRO_VERSION"

AM_INIT_AUTOMAKE

AC_SUBST(RAYGAY_MAJOR_VERSION)
AC_SUBST(RAYGAY_MINOR_VERSION)
AC_SUBST(RAYGAY_MICRO_VERSION)

dnl PACKAGE=tracer
dnl VERSION=0.1

dnl -------------------------------------------------
dnl Check that the compiler and programs exists
dnl -------------------------------------------------
AC_PROG_CXX
AC_PROG_LIBTOOL

dnl -------------------------------------------------
dnl Check system and headers
dnl -------------------------------------------------
AC_C_BIGENDIAN
AC_C_INLINE
AC_HEADER_TIME
AC_C_LONG_DOUBLE
AC_HEADER_DIRENT
AC_CHECK_HEADERS(pthread.h)
AC_CHECK_HEADERS(jpeglib.h)    
AC_CHECK_HEADERS(png.h)    

target_os=`uname`

dnl -------------------------------------------------
dnl Linux 
dnl -------------------------------------------------
if test "x$target_os" = "xLinux"; then
   AC_DEFINE([OS_LINUX],[1],[System is Linux])
   CXXFLAGS="-pthread $CXXFLAGS"  
fi
AM_CONDITIONAL(OS_LINUX, test "x$target_os" = "xLinux")


dnl -------------------------------------------------
dnl Mac OS X 
dnl -------------------------------------------------
if test "x$target_os" = "xDarwin"; then
   AC_CHECK_HEADERS([ApplicationServices/ApplicationServices.h],[],[],[])
   AC_DEFINE([OS_DARWIN],[1],[System is Mac OS X])
   DARWIN_LIBS="-framework ApplicationServices -framework Carbon"
   AC_SUBST(DARWIN_LIBS)
fi
AM_CONDITIONAL(OS_DARWIN, test "x$target_os" = "xDarwin")

dnl -------------------------------------------------
dnl Image libs
dnl -------------------------------------------------
if test "x$target_os" = "xLinux"; then
   AC_CHECK_LIB(jpeg, jpeg_read_header,JPEG_LIBS="-ljpeg")
   AC_CHECK_LIB(png, png_read_info, PNG_LIBS="-lpng")
   AC_SUBST(JPEG_LIBS)
   AC_SUBST(PNG_LIBS)
fi

dnl -------------------------------------------------
dnl Check libraries needed
dnl -------------------------------------------------
LIBS="$LIBS -lm"    
AC_CHECK_LIB(m, sin,,[AC_MSG_ERROR([math library is not found])],)
AC_CHECK_LIB(pthread, pthread_create,LIBS="-lpthread $LIBS")

dnl trunc,fmax,fmin are C99 functions and not available on older systems
AC_CHECK_FUNCS(trunc fmax fmin)

dnl AC_CHECK_LIB([mpi], [MPI_Init], 
dnl	[MPI_LIBS=`mpicc --showme:link`
dnl	 MPI_CFLAGS=`mpicc --showme:compile`
dnl	 AC_SUBST(MPI_LIBS)
dnl	 AC_SUBST(MPI_CFLAGS)
dnl	 AM_CONDITIONAL([HAVE_MPI], [1], [Define if you have mpi])
dnl	 AC_DEFINE([HAVE_MPI], [1], [Define if you have mpi])])

dnl -------------------------------------------------
dnl Check for MPI library. 
dnl -------------------------------------------------

ACX_MPI([AC_DEFINE([HAVE_MPI], 1, [Defined if you have libmpi])
      with_mpi=yes
      MPI_LIBS=`mpicxx --showme:link`
      AC_SUBST(MPI_LIBS)
      MPI_CXXFLAGS=`mpicxx --showme:compile`
      AC_SUBST(MPI_CXXFLAGS)],
      with_mpi=no)
AM_CONDITIONAL(HAVE_MPI, test "x$with_mpi" = "xyes")

dnl -------------------------------------------------
dnl Check for readline. Macro taken from autoconf manual.
dnl -------------------------------------------------
AC_ARG_WITH([readline],
	[AS_HELP_STRING([--with-readline],
  	[support fancy command line editing @<:@default=check@:>@])],
  [],
  [with_readline=check])
          
LIBREADLINE=
AS_IF([test "x$with_readline" != xno],
  [AC_CHECK_LIB([readline], [main],
    [AC_SUBST([LIBREADLINE], ["-lreadline -lncurses"])
     AC_DEFINE([HAVE_LIBREADLINE], [1],
               [Define if you have libreadline])
    ],
    [if test "x$with_readline" != xcheck; then
       AC_MSG_FAILURE(
         [--with-readline was given, but test for readline failed])
     fi
    ], -lncurses)])

dnl -------------------------------------------------
dnl Check for GTK
dnl -------------------------------------------------
PKG_PROG_PKG_CONFIG
PKG_CHECK_MODULES(GTK, [gtk+-2.0 >= 1.3],[gtk_found="yes"],[gtk_found="no"]) 
AC_MSG_RESULT([$gtk_found])
if test "x$gtk_found" = "xyes"; then
   AC_DEFINE([HAVE_GTK],[1],[Found GTK+-2.0. Will build the preview window.])
   AC_SUBST(GTK_CFLAGS)
   AC_SUBST(GTK_LIBS)
   GTK_LIBS="$GTK_LIBS -lgthread-2.0"
fi

dnl -------------------------------------------------
dnl Add the CPU dependent compile optimizations
dnl -------------------------------------------------
CXXFLAGS="-g -O3 -Wall -Winline -ffast-math $GTK_CFLAGS $GUILE_CFLAGS $CXXFLAGS"  
case $target in
     pentium3) CXXFLAGS="$CXXFLAGS -malign-double -march=pentium3 -mfpmath=sse -ftracer -mno-ieee-fp -fstrict-aliasing";;
     pentium4) CXXFLAGS="$CXXFLAGS -fprefetch-loop-arrays -funroll-loops -malign-double -fomit-frame-pointer -march=pentium4 -mfpmath=sse -ftracer -mno-ieee-fp -fstrict-aliasing -msse2";;
     g4) CXXFLAGS="$CXXFLAGS -ftracer -fstrict-aliasing";;
esac

dnl -------------------------------------------------
dnl Extra warning flags
dnl -------------------------------------------------
AC_MSG_CHECKING(whether --enable-extra-warnings is specified)
AC_ARG_ENABLE(extra-warnings, 
  [  --enable-extra-warnings        Enable extreme gcc-warnings.],
  [extra_warnings="yes"], [extra_warnings="no"])
if test "$extra_warnings" = yes; then
#   CXXFLAGS="$CXXFLAGS -W -Wunused -Wpadded -Waggregate-return"
   CXXFLAGS="$CXXFLAGS -W -Wunused -Wpadded -Wconversion" 
fi
AC_MSG_RESULT($extra_warnings)

dnl -------------------------------------------------
dnl Set C++ langage version
dnl -------------------------------------------------
CXXFLAGS="$CXXFLAGS -std=c++11"

dnl -------------------------------------------------
dnl Code coverage testing
dnl http://developer.apple.com/documentation/Darwin/Reference/ManPages/man1/gcov.1.html
dnl -------------------------------------------------
AC_MSG_CHECKING(whether --enable-gcov is specified)
AC_ARG_ENABLE(gcov, 
  [  --enable-gcov       Enable GNU code coverage testing.],
  [build_gcov="yes"], [build_gcov="no"])
if test "$build_gcov" = yes; then
   CXXFLAGS="$CXXFLAGS -fno-inline -O2 -fprofile-arcs -ftest-coverage" 
fi
AC_MSG_RESULT($build_gcov)

dnl -------------------------------------------------
dnl Enable profiling
dnl -------------------------------------------------
AC_MSG_CHECKING(whether --enable-profile is specified)
AC_ARG_ENABLE(profile, 
  [  --enable-profile        Build a debugging version.],
  [build_profile="yes"], [build_profile="no"])
if test "$build_profile" = yes; then
   CXXFLAGS="$CXXFLAGS -pg -O2 -fno-inline"
   LDFLAGS="$LDFLAGS -pg"
fi
AC_MSG_RESULT($build_profile)

dnl -------------------------------------------------
dnl Write the makefiles
dnl -------------------------------------------------
AC_OUTPUT(Makefile \
	  src/Makefile \
	  src/materials/Makefile \
	  src/materials/normalperturbers/Makefile \
	  src/paths/Makefile \
	  src/lights/Makefile \
	  src/math/Makefile \
	  src/image/Makefile \
	  src/imagefilters/Makefile \
	  src/parser/Makefile \
	  src/photon/Makefile \
	  src/objects/Makefile \
	  src/space/Makefile \
	  src/cameras/Makefile \
	  src/shaders/Makefile \
	  src/collections/Makefile \
	  src/samplers/Makefile \
	  src/filters/Makefile \
	  src/http/Makefile \
	  src/scheme/Makefile \
	  scenes/Makefile	\
	  scenes/lib/Makefile	\
	  docs/Makefile	\
	  test/Makefile		\
	  test/gfx/Makefile	\
	  test/scheme/Makefile)

echo "--------------------------------------------------------------"
echo "You can specify the target CPU to enable special optimizations"
echo "Example: ./configure --target=<cpu>"
echo
echo "Where <cpu> can be: pentium3, pentiu4, g4"
echo 
echo "--------------------------------------------------------------"


