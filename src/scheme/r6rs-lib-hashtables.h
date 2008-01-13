
#ifndef R6RS_LIB_HASHTABLES_H
#define R6RS_LIB_HASHTABLES_H

#include "scheme.h"

/**
 * Implementation of 
 *
 * (rnrs hashtables (6))
 */
struct R6RSLibHashtables {
    static void bind(Scheme* scheme, SchemeObject* envt);
};

#endif

