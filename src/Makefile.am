
SUBDIRS = materials paths lights math image #tests

#--------------------------------------

noinst_LIBRARIES = libtracer.a

libtracer_a_SOURCES = camera.cpp sphere.cpp intersection.cpp ray.cpp scene.cpp boolean.cpp box.cpp object.cpp mesh.cpp tetrahedron.cpp triangle.cpp cylinder.cpp pixelstore.cpp renderer.cpp raytracer.cpp necklace.cpp tessalation.cpp boundingbox.cpp bsp.cpp hierarchy.cpp

libtracer_a_LIBADD = paths/libpaths.a lights/liblights.a materials/libmaterials.a math/libmath.a image/libimage.a

#--------------------------------------

bin_PROGRAMS = tracer

tracer_SOURCES = main.cpp 

tracer_LDADD = libtracer.a paths/libpaths.a lights/liblights.a materials/libmaterials.a math/libmath.a image/libimage.a

#--------------------------------------

EXTRA_DIST = boolean.h 		\
             boundingbox.h	\
     	     bsp.h		\
             camera.h 		\
	     constants.h 	\
	     cylinder.h		\
	     hierarchy.h	\
	     intersection.h 	\
	     mesh.h		\
	     necklace.h		\
	     objectcollection.h	\
	     pixelstore.h	\
	     ray.h 		\
	     raytracer.h 	\
	     renderer.h 	\
	     scene.h 		\
	     spacesubdivider.h	\
	     sphere.h		\
	     tessalation.h	\
	     tetrahedron.h	\
             transformable.h 	\	
	     triangle.h		

run:
	sh test.sh
