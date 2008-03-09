
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

SchemeObject* s_bytevector_equal_p(Scheme*, SchemeObject* b1, SchemeObject* b2);
SchemeObject* s_u8_list_2_bytevector(Scheme*, SchemeObject* l);

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

#define uint8toint8(b) ((b) > 127 ? (b) - 256 : (b))
#define int8touint8(c) ((c) < 0 ? 256 + (c) : (c));

#endif

