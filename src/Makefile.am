
SUBDIRS = materials paths lights math image photon imagefilters objects space shaders parser cameras collections samplers filters


#--------------------------------------

lib_LTLIBRARIES = libtracer.la

libtracer_la_SOURCES = \
		      exception.h	\
		      exception.cpp	\
		      intersection.cpp 	\
		      ray.cpp 		\
		      scene.cpp 	\
		      renderer.cpp 	\
		      raytracer.cpp 	\
		      aabox.cpp 	\
		      stats.cpp 	\
		      photonrenderer.cpp \
		      photonmapdump.cpp \
		      renderjobs.cpp 	\
		      pathtracer.cpp 	\
		      transformer.cpp	\
		      transformer.h	\
		      environment.cpp 	\
		      window.h		

libtracer_la_LIBADD =  samplers/libsamplers.la parser/libparser.la photon/libphoton.la lights/liblights.la materials/libmaterials.la collections/libcollections.la objects/libobjects.la paths/libpaths.la math/libmath.la  image/libimage.la space/libspace.la shaders/libshaders.la cameras/libcameras.la imagefilters/libimagefilters.la filters/libfilters.la

#--------------------------------------
LIBS = -framework ApplicationServices		     

bin_PROGRAMS = tracer 
noinst_PROGRAMS = bench win

tracer_SOURCES = main.cpp window-gtk.cpp window-gtk.h window-icon.h
#tracer_CXXFLAGS = $(GTK_CFLAGS) $(CXXFLAGS)
tracer_LDADD = libtracer.la @GTK_LIBS@

#--------------------------------------

bench_SOURCES = bench-main.cpp
bench_LDADD = libtracer.la

if OS_DARWIN
   win_SOURCES = win-main.cpp
   win_LDADD = libtracer.la 
   win_LDFLAGS = -framework ApplicationServices
endif

#--------------------------------------

EXTRA_DIST = aabox.h	\
	     edgekey.h		\
	     environment.h	\
	     intersection.h 	\
	     pathtracer.h	\
	     photonmapdump.h	\
	     photonrenderer.h	\
	     ray.h 		\
	     raytracer.h 	\
	     renderer.h 	\
	     renderersettings.h	\
	     renderjobs.h	\
	     scene.h 		\
	     stats.h		\
	     types.h

