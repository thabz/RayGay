
SUBDIRS = materials paths lights math image photon filters objects

#--------------------------------------

noinst_LIBRARIES = libtracer.a

libtracer_a_SOURCES = camera.cpp intersection.cpp ray.cpp scene.cpp renderer.cpp raytracer.cpp boundingbox.cpp bsp.cpp stats.cpp importer.cpp photonrenderer.cpp photonmapdump.cpp kdtree.cpp 

libtracer_a_LIBADD = photon/libphoton.a lights/liblights.a materials/libmaterials.a math/libmath.a image/libimage.a  objects/libobjects.a paths/libpaths.a 

#--------------------------------------

bin_PROGRAMS = tracer

tracer_SOURCES = main.cpp 

tracer_LDADD = libtracer.a photon/libphoton.a lights/liblights.a materials/libmaterials.a math/libmath.a image/libimage.a  filters/libfilters.a objects/libobjects.a paths/libpaths.a 

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

