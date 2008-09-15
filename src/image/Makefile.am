
INCLUDES = -I$(top_srcdir)/src 

noinst_LTLIBRARIES = libimage.la

LIBS = @DARWIN_LIBS@ 

libimage_la_SOURCES = image.cpp \
                     rgb.cpp \
                     rgba.cpp \
                     texture.cpp \
		     simpletexture.cpp \
		     multitexture.cpp \
                     imageio.cpp \
		     imageio_tga.cpp \
		     imageio_hdri.cpp \
		     imageio_jpeg.cpp \
		     imageio_png.cpp \
		     imageio_darwin.cpp \
		     imagedrawing.cpp

libimage_la_LIBADD = @JPEG_LIBS@ @PNG_LIBS@
libimage_la_LDFLAGS = @DARWIN_LIBS@ 

EXTRA_DIST = image.h 		\
	imageio.h 		\
	imageio_jpeg.h	\
	imageio_png.h	\
	imageio_hdri.h	\
	imageio_tga.h 	\
	imageio_darwin.h	\
	imagedrawing.h	\
	rgba.h 		\
	rgb.h 		\
	texture.h 	        \
	simpletexture.h    \
	multitexture.h

