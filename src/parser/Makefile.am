
INCLUDES = -I$(top_srcdir)/src 

# Make bison generate the token header file parser.tab.h
AM_YFLAGS = -d

noinst_LIBRARIES = libparser.a

libparser_a_SOURCES = parser.yy		 tokenizer.ll	lightnodes.cpp 	assignments.cpp parserclass.cpp objectcollector.cpp function.cpp materialnodes.cpp langnodes.cpp sceneobjectnodes.cpp

BUILT_SOURCES = parser.h parserclass.h objectcollector.h


