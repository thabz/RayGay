
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

#define assert_arg_input_port_type(procname, argnum, arg) {     \
    if (i_input_port_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting input-port) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#define assert_arg_binary_port_type(procname, argnum, arg) {     \
    if (i_binary_port_p(arg) == S_FALSE) {                       \
        wostringstream ss;                                 \
        ss << "Wrong argument-type (expecting binary port) in position ";         \
        ss << argnum;                                     \
        ss << " in call to ";                             \
        ss << wstring(procname);                           \
        ss << ": " << (arg)->toString();                    \
        throw scheme_exception(ss.str());                 \
    }                                                     \
}

#endif

