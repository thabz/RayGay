
bin_PROGRAMS = tracer

tracer_SOURCES = main.cpp camera.cpp sphere.cpp vector.cpp matrix.cpp intersection.cpp ray.cpp scene.cpp image.cpp rgb.cpp material.cpp boolean.cpp perlin.cpp box.cpp boxobject.cpp hierarchy.cpp object.cpp mesh.cpp spotlight.cpp pointlight.cpp

INCLUDES=$(MAGIC_CFLAGS)
LIBS=$(MAGIC_LIBS)    

EXTRA_DIST = boolean.h 		\
	     box.h 		\
	     boxobject.h	\
             camera.h 		\
	     constants.h 	\
	     hierarchy.h 	\
	     intersection.h 	\
	     image.h		\
	     lightsource.h 	\
	     material.h 	\
	     matrix.h 		\
	     mesh.h		\
	     perlin.h 		\
	     pointlight.h	\
	     ray.h scene.h 	\
	     sphere.h		\
	     spotlight.h	\
             transformable.h 	\	
	     vector.h 		

run:
	sh test.sh
