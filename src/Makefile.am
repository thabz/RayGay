
SUBDIRS = materials paths lights

bin_PROGRAMS = tracer

tracer_SOURCES = main.cpp camera.cpp sphere.cpp vector.cpp matrix.cpp intersection.cpp ray.cpp scene.cpp image.cpp rgb.cpp boolean.cpp perlin.cpp boundingbox.cpp box.cpp hierarchy.cpp object.cpp mesh.cpp tetrahedron.cpp triangle.cpp cylinder.cpp pixelstore.cpp renderer.cpp raytracer.cpp bsp.cpp necklace.cpp tessalation.cpp

tracer_LDADD = materials/libmaterials.la \
	       paths/libpaths.la \
	       lights/liblights.la

EXTRA_DIST = boolean.h 		\
	     boundingbox.h 	\
	     bsp.h 		\
             camera.h 		\
	     constants.h 	\
	     cylinder.h		\
	     hierarchy.h 	\
	     intersection.h 	\
	     image.h		\
	     matrix.h 		\
	     mesh.h		\
	     necklace.h		\
	     objectcollection.h	\
	     perlin.h 		\
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
	     triangle.h		\
	     vector.h 		

run:
	sh test.sh
