
SUBDIRS = materials paths lights math image photon filters objects space shaders parser cameras


#--------------------------------------

noinst_LIBRARIES = libtracer.a

libtracer_a_SOURCES = \
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
		      window.cpp	\
		      window-icon.h	

libtracer_a_LIBADD = parser/libparser.a photon/libphoton.a lights/liblights.a materials/libmaterials.a  objects/libobjects.a paths/libpaths.a math/libmath.a  image/libimage.a space/libspace.a shaders/libshaders.a cameras/libcameras.a

#--------------------------------------

bin_PROGRAMS = tracer

tracer_SOURCES = main.cpp

tracer_LDADD =  parser/libparser.a 	\
		libtracer.a 		\
    		space/libspace.a 	\
    		photon/libphoton.a 	\
		lights/liblights.a 	\
		materials/libmaterials.a \
		math/libmath.a 		\
		filters/libfilters.a 	\
		objects/libobjects.a 	\
		shaders/libshaders.a  	\
		paths/libpaths.a  	\
		cameras/libcameras.a  	\
		image/libimage.a

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

