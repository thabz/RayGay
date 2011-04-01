
#ifndef COMPILER_H
#define COMPILER_H

#include "scheme.h"

/**
 *  The native code compiler
 */
struct Compiler {
    static void bind(Scheme* scheme, SchemeObject* envt);
};

#endif
