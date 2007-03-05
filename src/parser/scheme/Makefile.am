
INCLUDES = -I$(top_srcdir)/src 

noinst_LTLIBRARIES = libscheme.la

libscheme_la_SOURCES = lexer.h lexer.cpp

noinst_PROGRAMS = repl

repl_SOURCES = repl-main.cpp
repl_LDADD = libscheme.la 
