
bin_PROGRAMS = tracer

tracer_SOURCES = main.cpp camera.cpp sphere.cpp vector.cpp matrix.cpp intersection.cpp ray.cpp scene.cpp image.cpp rgb.cpp material.cpp boolean.cpp perlin.cpp boundingbox.cpp box.cpp hierarchy.cpp object.cpp mesh.cpp spotlight.cpp pointlight.cpp tetrahedron.cpp triangle.cpp path.cpp linesegment.cpp circle.cpp cylinder.cpp arealight.cpp pixelstore.cpp renderer.cpp raytracer.cpp

INCLUDES=$(MAGIC_CFLAGS)
LIBS=$(MAGIC_LIBS)    

EXTRA_DIST = arealight.h	\
    	     boolean.h 		\
	     boundingbox.h 	\
             camera.h 		\
	     constants.h 	\
	     cylinder.h		\
	     hierarchy.h 	\
	     intersection.h 	\
	     image.h		\
	     lightsource.h 	\
	     material.h 	\
	     matrix.h 		\
	     mesh.h		\
	     path.h		\
	     perlin.h 		\
	     pointlight.h	\
	     pixelstore.h	\
	     ray.h 		\
	     raytracer.h 	\
	     renderer.h 	\
	     scene.h 		\
             spacesubdivider.h	\
	     sphere.h		\
	     spotlight.h	\
	     tetrahedron.h	\
             transformable.h 	\	
	     triangle.h		\
	     vector.h 		

run:
	sh test.sh
