
INCLUDES = -I$(top_srcdir)/src 

noinst_LTLIBRARIES = libparser.la

libparser_la_SOURCES = pathfactory.cpp		\
		       pathfactory.h		\
		       camerafactory.cpp	\
		       camerafactory.h		\
		       parser.cpp		\
		       parser.h			\
		       sceneobjectfactory.h	\
		       sceneobjectfactory.cpp	\
		       lightsourcefactory.h	\
		       lightsourcefactory.cpp	\
		       transformationfactory.h	\
		       transformationfactory.cpp \
		       materialfactory.h	\
		       materialfactory.cpp	\
		       texturefactory.h		\
		       texturefactory.cpp	\
		       mathfactory.h		\
		       mathfactory.cpp		\
                       converters.h		\
                       converters.cpp		\
		       wrapper.cpp		\
		       wrapper.h

libparser_la_LIBADD = @GUILE_LDFLAGS@		       
