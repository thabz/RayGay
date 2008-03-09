
#include "r6rs-lib-io-ports.h"
#include "r6rs-lib-io-common.h"
#include "r6rs-lib-bytevectors.h"
#include "scheme.h"

SchemeObject* i_port_p(SchemeObject* obj) {
    SchemeObject::ObjectType type = obj->type();
    return type == SchemeObject::INPUT_PORT || type == SchemeObject::OUTPUT_PORT ? S_TRUE : S_FALSE; 
}

SchemeObject* s_port_p(Scheme* scheme, SchemeObject* obj) {
    return i_port_p(obj);
}

SchemeObject* i_textual_port_p(SchemeObject* obj) {
    return i_port_p(obj) == S_TRUE && obj->textual() ? S_TRUE : S_FALSE;
}

SchemeObject* s_textual_port_p(Scheme* scheme, SchemeObject* obj) {
    return i_textual_port_p(obj);
}

SchemeObject* i_binary_port_p(SchemeObject* obj) {
    return i_port_p(obj) == S_TRUE && !obj->textual() ? S_TRUE : S_FALSE;
}

SchemeObject* s_binary_port_p(Scheme* scheme, SchemeObject* obj) {
    return i_binary_port_p(obj);
}

SchemeObject* s_open_bytevector_input_port(Scheme* scheme, SchemeObject* bytevector, SchemeObject* maybe_transcoder) {
    assert_arg_bytevector_type(L"open-bytevector-input-port", 1, bytevector);
    char* bytes = (char*) bytevector->bytevector;
    uint32_t length = bytevector->length;
    string s = string(bytes,length);
    istringstream* iss = new istringstream(s, istringstream::in|istringstream::binary);
    SchemeObject* port = SchemeObject::createInputPort(iss);
    port->set_textual(false);
    return port;    
}

// Returns -1 if EOF, otherwise an int in the range [0,255].
int32_t i_get_u8(SchemeObject* binary_input_port) {
    return binary_input_port->is->get();
}

SchemeObject* s_get_u8(SchemeObject* scheme, SchemeObject* port) {
    assert_arg_input_port_type(L"get-u8", 1, port);
    assert_arg_binary_port_type(L"get-u8", 1, port);
    int32_t c = i_get_u8(port);
    if (c == -1) {
        return S_EOF;
    } else {
        return int2scm(c);
    }
}

// Returns -1 if EOF, otherwise an int in the range [0,255].
int32_t i_lookahead_u8(SchemeObject* binary_input_port) {
    return binary_input_port->is->peek();
}

SchemeObject* s_lookahead_u8(SchemeObject* scheme, SchemeObject* port) {
    assert_arg_input_port_type(L"lookahead-u8", 1, port);
    assert_arg_binary_port_type(L"lookahead-u8", 1, port);
    int32_t c = i_lookahead_u8(port);
    if (c == -1) {
        return S_EOF;
    } else {
        return int2scm(c);
    }
}

void R6RSLibIOPorts::bind(Scheme* scheme, SchemeObject* envt) {
	scheme->assign(L"port?"                 ,1,0,0, (SchemeObject* (*)()) s_port_p, envt);
	scheme->assign(L"textual-port?"         ,1,0,0, (SchemeObject* (*)()) s_textual_port_p, envt);
	scheme->assign(L"binary-port?"          ,1,0,0, (SchemeObject* (*)()) s_binary_port_p, envt);
	scheme->assign(L"open-bytevector-input-port"
	                                        ,1,1,0, (SchemeObject* (*)()) s_open_bytevector_input_port, envt);
	scheme->assign(L"get-u8"                ,1,0,0, (SchemeObject* (*)()) s_get_u8, envt);
	scheme->assign(L"lookahead-u8"          ,1,0,0, (SchemeObject* (*)()) s_lookahead_u8, envt);

    // Defined in io-common
	scheme->assign(L"current-input-port"    ,0,0,0, (SchemeObject* (*)()) s_current_input_port, envt);
	scheme->assign(L"current-output-port"   ,0,0,0, (SchemeObject* (*)()) s_current_output_port, envt);
	scheme->assign(L"input-port?"           ,1,0,0, (SchemeObject* (*)()) s_input_port_p, envt);
	scheme->assign(L"output-port?"          ,1,0,0, (SchemeObject* (*)()) s_output_port_p, envt);
	scheme->assign(L"eof-object?"           ,1,0,0, (SchemeObject* (*)()) s_eof_object_p, envt);
}
