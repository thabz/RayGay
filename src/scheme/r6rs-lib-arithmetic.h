
#ifndef R6RS_LIB_ARITHMETIC_H
#define R6RS_LIB_ARITHMETIC_H

#include "scheme.h"

/**
 * Implementation of 
 *
 * (rnrs arithmetic fixnums (6))
 * (rnrs arithmetic flonums (6))
 * (rnrs arithmetic bitwise (6))
 */
struct R6RSLibArithmetic {
    static void bind(Scheme* scheme, SchemeObject* envt);
};

#endif

