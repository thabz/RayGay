
#ifndef R6RS_LIB_IO_SIMPLE_H
#define R6RS_LIB_IO_SIMPLE_H

#include "scheme.h"

/**
 * Implementation of 
 *
 * (rnrs io simple (6))
 */
struct R6RSLibIOSimple {
    static void bind(Scheme* scheme, SchemeObject* envt);
};

SchemeObject* s_open_input_file(Scheme* scheme, SchemeObject* s_filename);
SchemeObject* s_open_output_file(Scheme* scheme, SchemeObject* s_filename);
SchemeObject* s_close_input_port(Scheme* scheme, SchemeObject* s_port);
SchemeObject* s_close_output_port(Scheme* scheme, SchemeObject* s_port);
SchemeObject* s_call_with_input_file(Scheme* scheme, SchemeObject* s_filename, SchemeObject* proc);
SchemeObject* s_call_with_output_file(Scheme* scheme, SchemeObject* s_filename, SchemeObject* proc);
SchemeObject* s_with_input_from_file(Scheme* scheme, SchemeObject* s_filename, SchemeObject* thunk);
SchemeObject* s_with_output_to_file(Scheme* scheme, SchemeObject* s_filename, SchemeObject* thunk);
SchemeObject* s_read_char(Scheme* scheme, SchemeObject* s_port);
SchemeObject* s_peek_char(Scheme* scheme, SchemeObject* s_port);
SchemeObject* s_write_char(Scheme* scheme, SchemeObject* s_char, SchemeObject* s_port);
SchemeObject* s_read(Scheme* scheme, SchemeObject* s_port);
SchemeObject* s_write(Scheme* scheme, SchemeObject* o, SchemeObject* port);
SchemeObject* s_display(Scheme* scheme, SchemeObject* o, SchemeObject* port); 
SchemeObject* s_newline(Scheme* scheme, SchemeObject* port );


#endif

