
SUBDIRS = materials paths lights math image photon filters objects space shaders parser cameras collections


#--------------------------------------

lib_LTLIBRARIES = libtracer.la

libtracer_la_SOURCES = \
		      exception.h	\
		      intersection.cpp 	\
		      ray.cpp 		\
		      scene.cpp 	\
		      renderer.cpp 	\
		      raytracer.cpp 	\
		      boundingbox.cpp 	\
		      stats.cpp 	\
		      photonrenderer.cpp \
		      photonmapdump.cpp \
		      renderjobs.cpp 	\
		      pathtracer.cpp 	\
		      environment.cpp 	\
		      window.h		\
		      window-gtk.h	\
		      window-gtk.cpp	\
		      window-icon.h	

libtracer_la_LIBADD = parser/libparser.la photon/libphoton.la lights/liblights.la materials/libmaterials.la collections/libcollections.la objects/libobjects.la paths/libpaths.la math/libmath.la  image/libimage.la space/libspace.la shaders/libshaders.la cameras/libcameras.la filters/libfilters.la

#--------------------------------------

bin_PROGRAMS = tracer bench

tracer_SOURCES = main.cpp

tracer_LDADD = libtracer.la 		

#--------------------------------------

bench_SOURCES = bench-main.cpp

bench_LDADD = libtracer.la

#--------------------------------------

EXTRA_DIST = boundingbox.h	\
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
	     stats.h		

