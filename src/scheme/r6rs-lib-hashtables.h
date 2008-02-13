
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

#define assert_arg_hashtable_type(procname, argnum, arg) {     \
    if (i_hashtable_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting hashtable) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_mutable_hashtable_type(procname, argnum, arg) {     \
    if (i_hashtable_p(arg) == S_FALSE || (arg)->immutable()) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting mutable hashtable) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#endif

