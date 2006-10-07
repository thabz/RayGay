
INCLUDES = -I$(top_srcdir)/src 

noinst_LTLIBRARIES = libimage.la

#LIBS = -framework ApplicationServices

libimage_la_SOURCES = image.cpp \
                     rgb.cpp \
                     rgba.cpp \
                     texture.cpp \
		     imageio_tga.cpp \
		     imageio_hdri.cpp \
		     imageio_jpeg.cpp \
		     imageio_png.cpp \
		     imageio_darwin.cpp

libimage_la_LIBADD = @JPEG_LIBS@ @PNG_LIBS@		     

EXTRA_DIST = image.h 		\
             imageio.h 		\
	     imageio_jpeg.h	\
	     imageio_png.h	\
	     imageio_hdri.h	\
             imageio_tga.h 	\
             imageio_darwin.h	\
             rgba.h 		\
             rgb.h 		\
             texture.h 	
