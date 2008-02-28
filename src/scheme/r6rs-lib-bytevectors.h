
#ifndef R6RS_LIB_BYTEVECTORS_H
#define R6RS_LIB_BYTEVECTORS_H

#include "scheme.h"

/**
 * Implementation of 
 *
 * (rnrs bytevectors (6))
 */
struct R6RSLibBytevectors {
    static void bind(Scheme* scheme, SchemeObject* envt);
};

#define assert_arg_bytevector_type(procname, argnum, arg) {     \
    if (i_bytevector_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting bytevector) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#endif

