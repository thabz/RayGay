INCLUDES = -I$(top_srcdir)/src 

# -----------------------------------------------------
# Library
# -----------------------------------------------------
noinst_LTLIBRARIES = libscheme.la
libscheme_la_SOURCES = lexer.h lexer.cpp \
                       interpreter.h interpreter.cpp \
                       objects.h objects.cpp \
                       parser.h parser.cpp \
                       heap.h heap.cpp \
                       scheme.h scheme.cpp

# -----------------------------------------------------
# Tests
# -----------------------------------------------------
check_PROGRAMS = testscheme
testscheme_SOURCES = testscheme.cpp
testscheme_LDADD = libscheme.la
TESTS = testscheme
 

