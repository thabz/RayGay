
SUBDIRS = materials paths lights math image photon

#--------------------------------------

noinst_LIBRARIES = libtracer.a

libtracer_a_SOURCES = camera.cpp sphere.cpp intersection.cpp ray.cpp scene.cpp boolean.cpp box.cpp object.cpp mesh.cpp tetrahedron.cpp triangle.cpp cylinder.cpp renderer.cpp raytracer.cpp necklace.cpp tessalation.cpp boundingbox.cpp bsp.cpp  stats.cpp extrusion.cpp wireframe.cpp objectgroup.cpp bezierpatch.cpp teapot.cpp 3ds.cpp importer.cpp torus.cpp photonrenderer.cpp photonmapdump.cpp kdtree.cpp sor.cpp

libtracer_a_LIBADD = photon/libphoton.a paths/libpaths.a lights/liblights.a materials/libmaterials.a math/libmath.a image/libimage.a 

#--------------------------------------

bin_PROGRAMS = tracer

tracer_SOURCES = main.cpp 

tracer_LDADD = libtracer.a photon/libphoton.a paths/libpaths.a lights/liblights.a materials/libmaterials.a math/libmath.a image/libimage.a 

#--------------------------------------

EXTRA_DIST = 3ds.h		\
    	     bezierpatch.h	\
    	     boolean.h 		\
             boundingbox.h	\
             booleanoperand.h	\
	     box.h		\
     	     bsp.h		\
             camera.h 		\
	     cylinder.h		\
	     edgekey.h		\
	     extrusion.h	\
	     importer.h		\
	     intersection.h 	\
	     kdtree.h		\
	     mesh.h		\
	     object.h		\
	     necklace.h		\
	     objectcollection.h	\
	     objectgroup.h	\
	     photonmap.h	\
	     photonmapdump.h	\
	     photonrenderer.h	\
	     photonsettings.h	\
	     photontracer.h	\
	     ray.h 		\
	     raytracer.h 	\
	     renderer.h 	\
	     scene.h 		\
	     sceneobject.h	\
	     sor.h		\
	     spacesubdivider.h	\
	     sphere.h		\
	     stats.h		\
	     teapot.h		\
	     tessalation.h	\
	     tetrahedron.h	\
	     torus.h		\
	     triangle.h 	\
	     wireframe.h

