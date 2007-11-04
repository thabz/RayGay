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
                       bigint.h bigint.cpp \
                       optimizer.h optimizer.cpp \
                       scheme.h scheme.cpp \
                       filenames.h filenames.cpp \
                       r6rs-lib-lists.h r6rs-lib-lists.cpp \
                       r6rs-lib-sorting.h r6rs-lib-sorting.cpp \
                       r6rs-lib-unicode.h r6rs-lib-unicode.cpp

# -----------------------------------------------------
# Tests
# -----------------------------------------------------
check_PROGRAMS = testscheme testrational
testscheme_SOURCES = testscheme.cpp
testscheme_LDADD = libscheme.la
testrational_SOURCES = testrational.cpp
TESTS = testscheme testrational
 

