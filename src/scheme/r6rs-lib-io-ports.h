
#ifndef R6RS_LIB_IO_PORTS_H
#define R6RS_LIB_IO_PORTS_H

#include "scheme.h"

/**
 * Implementation of 
 *
 * (rnrs io ports (6))
 */
struct R6RSLibIOPorts {
    static void bind(Scheme* scheme, SchemeObject* envt);
};

#endif

