
INCLUDES = -I$(top_srcdir)/src 

# Make bison generate the token header file parser.tab.h
AM_YFLAGS = -d

noinst_LIBRARIES = libparser.a

libparser_a_SOURCES = \
		      parser.yy		\
		      tokenizer.ll	\
		      assignments.h	\
		      assignments.cpp 	\
		      boolnodes.h	\
		      cameranode.h	\
		      fileposition.h	\
		      floatnodes.h	\
		      function.h	\
		      function.cpp 	\
		      langnodes.h	\
		      langnodes.cpp 	\
		      lightnodes.h	\
		      lightnodes.cpp	\
		      materialnodes.h	\
		      materialnodes.cpp \
		      objectcollector.h	\
		      objectcollector.cpp \
		      parserclass.h	\
		      parserclass.cpp 	\
		      pathnodes.h	\
		      rgbnodes.h	\
		      runtimeexception.h \
		      sceneobjectnodes.h \
		      sceneobjectnodes.cpp \
		      syntaxnode.h	\
		      transformationnodes.h	\
		      vectornodes.h

BUILT_SOURCES = parser.h parserclass.h objectcollector.h


