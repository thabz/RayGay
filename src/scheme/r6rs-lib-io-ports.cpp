
#include "r6rs-lib-io-ports.h"
#include "r6rs-lib-io-common.h"
#include "r6rs-lib-bytevectors.h"
#include "scheme.h"

#define CODEC_UTF8   1
#define CODEC_LATIN1 2
#define CODEC_UTF16  3
#define EOL_STYLE_LF    0 
#define EOL_STYLE_CR    1
#define EOL_STYLE_CRLF  2
#define EOL_STYLE_NEL   3
#define EOL_STYLE_CRNEL 4
#define EOL_STYLE_LS    5
#define ERROR_HANDLING_MODE_REPLACE 0
#define ERROR_HANDLING_MODE_IGNORE  1
#define ERROR_HANDLING_MODE_RAISE   2



SchemeObject* latin1_codec;
SchemeObject* utf8_codec;
SchemeObject* utf16_codec;

SchemeObject* s_latin_1_codec(Scheme* scheme) {
    return latin1_codec;
}

SchemeObject* s_utf_8_codec(Scheme* scheme) {
    return utf8_codec;
}

SchemeObject* s_utf_16_codec(Scheme* scheme) {
    return utf16_codec;
}


SchemeObject* s_make_transcoder(Scheme* scheme, SchemeObject* codec, SchemeObject* eol_style, SchemeObject* handling_mode) {
    if (eol_style == S_UNSPECIFIED) {
        eol_style = SchemeObject::createSymbol(L"crlf");
    } 
    if (handling_mode == S_UNSPECIFIED) {
        handling_mode = SchemeObject::createSymbol(L"replace");
    }
    return i_list_3(codec, eol_style, handling_mode);
}

SchemeObject* s_transcoder_codec(Scheme* scheme, SchemeObject* transcoder) {
    return s_car(scheme,transcoder);
}

SchemeObject* s_transcoder_eol_style(Scheme* scheme, SchemeObject* transcoder) {
    return s_cadr(scheme,transcoder);
}

SchemeObject* s_transcoder_error_handling_mode(Scheme* scheme, SchemeObject* transcoder) {
    return s_caddr(scheme,transcoder);
}


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

SchemeObject* s_port_eof_p(Scheme* scheme, SchemeObject* obj) {
    if (i_input_port_p(obj) == S_TRUE) {
        return obj->is->eof() || obj->is->peek() == -1 ? S_TRUE : S_FALSE;
    }
    return S_FALSE;
}

SchemeObject* s_transcoded_port(Scheme* scheme, SchemeObject* port, SchemeObject* transcoder) {
    SchemeObject* newport;
    if (i_input_port_p(port) == S_TRUE) {
        newport = SchemeObject::createInputPort(port->is);
    } else {
        newport = SchemeObject::createOutputPort(port->os);
    }
    newport->set_textual(true);
    newport->transcoder = transcoder;
    return newport;
}

SchemeObject* s_open_bytevector_input_port(Scheme* scheme, SchemeObject* bytevector, SchemeObject* maybe_transcoder) {
    assert_arg_bytevector_type(L"open-bytevector-input-port", 1, bytevector);
    char* bytes = (char*) bytevector->bytevector;
    uint32_t length = bytevector->length;
    string s = string(bytes,length);
    istringstream* iss = new istringstream(s, istringstream::in|istringstream::binary);
    SchemeObject* port = SchemeObject::createInputPort(iss);
    
    if (maybe_transcoder == S_FALSE || maybe_transcoder == S_UNSPECIFIED) {
        port->set_textual(false);
    } else {
        port->set_textual(true);
        port->transcoder = maybe_transcoder;
    }
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

// Returns -1 if EOF, otherwise an int in the range [0,255].
wchar_t i_get_char(SchemeObject* textual_input_port) {
    return textual_input_port->transcoder->car->codec->get(textual_input_port->is);
}

SchemeObject* s_get_char(Scheme* scheme, SchemeObject* port) {
    assert_arg_input_port_type(L"get-char", 1, port);
    assert_arg_textual_port_type(L"get-char", 1, port);
    wchar_t c = i_get_char(port);
    return c == -1 ? S_EOF : char2scm(c);
}

void R6RSLibIOPorts::bind(Scheme* scheme, SchemeObject* envt) {
    latin1_codec = SchemeObject::createCodec(new Latin1Codec());
    utf8_codec = SchemeObject::createCodec(new UTF8Codec());
    utf16_codec = SchemeObject::createCodec(new UTF16Codec());
    
	scheme->assign(L"latin-1-codec"         ,0,0,0, (SchemeObject* (*)()) s_latin_1_codec, envt);
	scheme->assign(L"utf-8-codec"           ,0,0,0, (SchemeObject* (*)()) s_utf_8_codec, envt);
	scheme->assign(L"utf-16-codec"          ,0,0,0, (SchemeObject* (*)()) s_utf_16_codec, envt);
	scheme->assign(L"make-transcoder"       ,1,2,0, (SchemeObject* (*)()) s_make_transcoder, envt);
	scheme->assign(L"transcoder-codec"      ,1,0,0, (SchemeObject* (*)()) s_transcoder_codec, envt);
	scheme->assign(L"transcoder-eol-style"  ,1,0,0, (SchemeObject* (*)()) s_transcoder_eol_style, envt);
	scheme->assign(L"transcoder-error-handling-mode"
	                                        ,1,0,0, (SchemeObject* (*)()) s_transcoder_error_handling_mode, envt);
	scheme->assign(L"transcoder-codec"      ,1,0,0, (SchemeObject* (*)()) s_transcoder_codec, envt);
	scheme->assign(L"port?"                 ,1,0,0, (SchemeObject* (*)()) s_port_p, envt);
	scheme->assign(L"textual-port?"         ,1,0,0, (SchemeObject* (*)()) s_textual_port_p, envt);
	scheme->assign(L"binary-port?"          ,1,0,0, (SchemeObject* (*)()) s_binary_port_p, envt);
	scheme->assign(L"transcoded-port"       ,2,0,0, (SchemeObject* (*)()) s_transcoded_port, envt);
	scheme->assign(L"port-eof?"             ,1,0,0, (SchemeObject* (*)()) s_port_eof_p, envt);
	scheme->assign(L"open-bytevector-input-port"
	                                        ,1,1,0, (SchemeObject* (*)()) s_open_bytevector_input_port, envt);
	scheme->assign(L"get-u8"                ,1,0,0, (SchemeObject* (*)()) s_get_u8, envt);
	scheme->assign(L"lookahead-u8"          ,1,0,0, (SchemeObject* (*)()) s_lookahead_u8, envt);
	scheme->assign(L"get-char"              ,1,0,0, (SchemeObject* (*)()) s_get_char, envt);

    // Defined in io-common
	scheme->assign(L"current-input-port"    ,0,0,0, (SchemeObject* (*)()) s_current_input_port, envt);
	scheme->assign(L"current-output-port"   ,0,0,0, (SchemeObject* (*)()) s_current_output_port, envt);
	scheme->assign(L"input-port?"           ,1,0,0, (SchemeObject* (*)()) s_input_port_p, envt);
	scheme->assign(L"output-port?"          ,1,0,0, (SchemeObject* (*)()) s_output_port_p, envt);
	scheme->assign(L"eof-object?"           ,1,0,0, (SchemeObject* (*)()) s_eof_object_p, envt);
}


#define LEN4_MASK 0xf8   // 11111000
#define LEN4_VAL  0xf0   // 11110000
#define LEN3_MASK 0xf0   // 11110000
#define LEN3_VAL  0xe0   // 11100000
#define LEN2_MASK 0xe0   // 11100000
#define LEN2_VAL  0xc0   // 11000000
#define SEVEN_BIT_MASK  0x7f  // 01111111
#define SIX_BIT_MASK    0x3f  // 00111111
#define FIVE_BIT_MASK   0x1f  // 00011111
#define FOUR_BIT_MASK   0x0f  // 00001111
#define THREE_BIT_MASK  0x07  // 00000111
#define TWO_BIT_MASK    0x03  // 00000011
#define ONE_BIT_MASK    0x01  // 00000001

wchar_t UTF8Codec::get(istream* is) {
    int32_t c = is->get();
    if (c == -1 || c < 128) {
        return c;
    } else if ((c & LEN2_MASK) == LEN2_VAL) {
        c = c & FIVE_BIT_MASK;
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
    } else if ((c & LEN3_MASK) == LEN3_VAL) {
        c = c & FOUR_BIT_MASK;
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
    } else if ((c & LEN4_MASK) == LEN4_VAL) {
        c = c & THREE_BIT_MASK;
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
    } else {
        // Invalid UTF-8 sequence
    }
    return c;
}

wchar_t UTF16Codec::get(istream* is) {
    int32_t c = is->get();
    if (c == -1 || c < 128) {
        return c;
    } else if ((c & LEN2_MASK) == LEN2_VAL) {
        c = c & FIVE_BIT_MASK;
        c |= is->get() & SIX_BIT_MASK;
    } else if ((c & LEN3_MASK) == LEN3_VAL) {
        c = c & FOUR_BIT_MASK;
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
    } else if ((c & LEN4_MASK) == LEN4_VAL) {
        c = c & THREE_BIT_MASK;
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
        c = (c << 6) | (is->get() & SIX_BIT_MASK);
    } else {
        // Invalid UTF-8 sequence
    }
    return c;
}

wchar_t Latin1Codec::get(istream* is) {
    int32_t c = is->get();
    return c;
}

