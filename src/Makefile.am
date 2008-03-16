
SUBDIRS = materials paths lights math image photon imagefilters objects space shaders parser cameras collections samplers filters http scheme


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
		      profiler.cpp	\
		      profiler.h	\
		      allocator.cpp	\
		      allocator.h	\
		      mmap_allocator.h	\
		      renderersettings.cpp \
		      ttf.cpp		\
		      gzip.cpp		\
		      window.h		

libtracer_la_LIBADD =  samplers/libsamplers.la parser/libparser.la photon/libphoton.la lights/liblights.la materials/libmaterials.la collections/libcollections.la objects/libobjects.la paths/libpaths.la math/libmath.la  image/libimage.la space/libspace.la shaders/libshaders.la cameras/libcameras.la imagefilters/libimagefilters.la filters/libfilters.la scheme/libscheme.la

#--------------------------------------
LIBS = @DARWIN_LIBS@		     

bin_PROGRAMS = raygay raygay-server raygay-queuemanager raygay-repl
noinst_PROGRAMS = bench 
if OS_DARWIN
   noinst_PROGRAMS += win
endif   

raygay_SOURCES = main.cpp window-gtk.cpp window-gtk.h window-icon.h
#tracer_CXXFLAGS = $(GTK_CFLAGS) $(CXXFLAGS)
raygay_LDADD = libtracer.la @GTK_LIBS@

#--------------------------------------

raygay_server_SOURCES = server-main.cpp
raygay_server_LDADD = libtracer.la http/libhttp.la

#--------------------------------------

raygay_queuemanager_SOURCES = queuemanager-main.cpp
raygay_queuemanager_LDADD = libtracer.la http/libhttp.la

#--------------------------------------

raygay_repl_SOURCES = repl-main.cpp
raygay_repl_LDADD = libtracer.la  @LIBREADLINE@

#--------------------------------------

bench_SOURCES = bench-main.cpp
bench_LDADD = libtracer.la

if OS_DARWIN
   win_SOURCES = win-main.cpp
   win_LDADD = libtracer.la 
   win_LDFLAGS = @DARWIN_LIBS@
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

