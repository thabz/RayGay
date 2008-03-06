
#ifndef R6RS_LIB_IO_COMMON_H
#define R6RS_LIB_IO_COMMON_H

#include "scheme.h"

SchemeObject* s_eof_object(Scheme*);
SchemeObject* s_eof_object_p(Scheme* scheme,SchemeObject* o);
SchemeObject* s_input_port_p(Scheme* scheme,SchemeObject* o);
SchemeObject* s_output_port_p(Scheme* scheme,SchemeObject* o);
SchemeObject* s_current_input_port(Scheme* scheme);
SchemeObject* s_current_output_port(Scheme* scheme);

#endif

