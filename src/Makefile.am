
SUBDIRS = materials paths lights math image photon filters objects

#--------------------------------------

noinst_LIBRARIES = libtracer.a

libtracer_a_SOURCES = camera.cpp intersection.cpp ray.cpp scene.cpp renderer.cpp raytracer.cpp boundingbox.cpp bsp.cpp stats.cpp importer.cpp photonrenderer.cpp photonmapdump.cpp kdtree.cpp #spacesubdivider.cpp

libtracer_a_LIBADD = photon/libphoton.a lights/liblights.a materials/libmaterials.a  objects/libobjects.a paths/libpaths.a math/libmath.a  image/libimage.a

#--------------------------------------

bin_PROGRAMS = tracer

tracer_SOURCES = main.cpp 

tracer_LDADD = libtracer.a photon/libphoton.a lights/liblights.a materials/libmaterials.a  math/libmath.a filters/libfilters.a objects/libobjects.a paths/libpaths.a  image/libimage.a

#--------------------------------------

EXTRA_DIST = boundingbox.h	\
     	     bsp.h		\
             camera.h 		\
	     edgekey.h		\
	     importer.h		\
	     intersection.h 	\
	     kdtree.h		\
	     photonmap.h	\
	     photonmapdump.h	\
	     photonrenderer.h	\
	     photonsettings.h	\
	     photontracer.h	\
	     ray.h 		\
	     raytracer.h 	\
	     renderer.h 	\
	     scene.h 		\
	     spacesubdivider.h	\
	     stats.h

