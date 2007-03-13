
INCLUDES = -I$(top_srcdir)/src 

noinst_LTLIBRARIES = libscheme.la

libscheme_la_SOURCES = lexer.h lexer.cpp \
                       interpreter.h interpreter.cpp \
                       objects.h objects.cpp \
                       parser.h parser.cpp \
                       scheme.h scheme.cpp \
                       binding-env.h binding-env.cpp
 

noinst_PROGRAMS = repl

repl_SOURCES = repl-main.cpp
repl_LDADD = libscheme.la 
