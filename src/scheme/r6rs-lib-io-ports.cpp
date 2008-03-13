
#include <fstream>

#include "r6rs-lib-io-ports.h"
#include "r6rs-lib-io-common.h"
#include "r6rs-lib-bytevectors.h"
#include "scheme.h"
#include "numbers.h"
#include "filenames.h"

SchemeObject* latin1_codec;
SchemeObject* utf8_codec;
SchemeObject* utf16_codec;
SchemeObject* rawunicode_codec;

SchemeObject* s_latin_1_codec(Scheme* scheme) {
    return latin1_codec;
}

SchemeObject* s_utf_8_codec(Scheme* scheme) {
    return utf8_codec;
}

SchemeObject* s_utf_16_codec(Scheme* scheme) {
    return utf16_codec;
}

SchemeObject* s_native_eol_style(Scheme* scheme) {
    return SchemeObject::createSymbol(L"crlf");
}

SchemeObject* s_make_transcoder(Scheme* scheme, SchemeObject* codec, SchemeObject* eol_style, SchemeObject* handling_mode) {
    if (eol_style == S_UNSPECIFIED) {
        eol_style = s_native_eol_style(scheme);
    } 
    if (handling_mode == S_UNSPECIFIED) {
        handling_mode = SchemeObject::createSymbol(L"replace");
    }
    return i_list_3(codec, eol_style, handling_mode);
}

SchemeObject* s_native_transcoder(Scheme* scheme) {
    SchemeObject* codec = s_utf_8_codec(scheme);
    return s_make_transcoder(scheme, codec, S_UNSPECIFIED, S_UNSPECIFIED);
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

SchemeObject* i_port_eof_p(SchemeObject* port) {
    if (i_textual_port_p(port) && port->transcoder != NULL) {
        return port->transcoder->car->codec->peek(port) == -1 ? S_TRUE : S_FALSE;
    } else {
        return port->is->eof() || port->is->peek() == -1 ? S_TRUE : S_FALSE;
    }
}

SchemeObject* s_port_eof_p(Scheme* scheme, SchemeObject* obj) {
    if (i_input_port_p(obj) == S_TRUE) {
        return i_port_eof_p(obj);
    }
    return S_FALSE;
}

SchemeObject* s_port_transcoder(Scheme* scheme, SchemeObject* port) {
    assert_arg_input_port_type(L"port-transcoder", 1, port);
    if (i_textual_port_p(port) == S_TRUE && port->transcoder != NULL) {
	    return port->transcoder;
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

SchemeObject* s_open_file_input_port(Scheme* scheme, SchemeObject* filename, SchemeObject* file_options, SchemeObject* buffer_mode, SchemeObject* maybe_transcoder) {
    assert_arg_string_type(L"open-file-input-port", 1, filename);
    ifstream* ifs = new ifstream(SchemeFilenames::toFilename(scm2string(filename)).c_str(), ios::in);
    if (ifs->fail()) {
        throw scheme_exception(L"Error opening file " + filename->toString());
    }
    if (file_options != S_FALSE || buffer_mode != S_FALSE) {
        cout << "open-file-inputport: Not support for file-options and buffer-mode. Arguments ignored." << endl;
    }
    SchemeObject* port = SchemeObject::createInputPort(ifs);
    if (maybe_transcoder == S_FALSE || maybe_transcoder == S_UNSPECIFIED) {
        port->set_textual(false);
    } else {
        port->set_textual(true);
        port->transcoder = maybe_transcoder;
    }
    return port;
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

SchemeObject* i_open_string_input_port(Scheme* scheme, wstring s) {
    wistringstream* wiss = new wistringstream(s, istringstream::in);
    SchemeObject* port = SchemeObject::createInputPort(wiss);
    port->set_textual(true);
    port->transcoder = s_make_transcoder(scheme, rawunicode_codec, S_UNSPECIFIED, S_UNSPECIFIED);
    return port;
}

SchemeObject* s_open_string_input_port(Scheme* scheme, SchemeObject* str) {
    assert_arg_string_type(L"open-string-input-port", 1, str);
    return i_open_string_input_port(scheme, scm2string(str));
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
    return textual_input_port->transcoder->car->codec->get(textual_input_port);
}

SchemeObject* s_get_char(Scheme* scheme, SchemeObject* port) {
    assert_arg_input_port_type(L"get-char", 1, port);
    assert_arg_textual_port_type(L"get-char", 1, port);
    wchar_t c = i_get_char(port);
    return c == -1 ? S_EOF : char2scm(c);
}

// Returns -1 if EOF, otherwise an int in the range [0,255].
wchar_t i_lookahead_char(SchemeObject* textual_input_port) {
    return textual_input_port->transcoder->car->codec->peek(textual_input_port);
}

SchemeObject* s_lookahead_char(Scheme* scheme, SchemeObject* port) {
    assert_arg_input_port_type(L"lookahead-char", 1, port);
    assert_arg_textual_port_type(L"lookahead-char", 1, port);
    wchar_t c = i_lookahead_char(port);
    return c == -1 ? S_EOF : char2scm(c);
}

void i_unget_char(SchemeObject* textual_input_port) {
    return textual_input_port->transcoder->car->codec->unget(textual_input_port);
}

SchemeObject* s_get_string_n(Scheme* scheme, SchemeObject* port, SchemeObject* count) {
    assert_arg_input_port_type(L"get-string-n", 1, port);
    assert_arg_textual_port_type(L"get-string-n", 1, port);
    assert_arg_positive_int(L"get-string-n", 2, count);
    int32_t n = scm2int(count);
    if (n == 0) {
        return string2scm(wstring(L""));
    }
    wchar_t chars[n+1];
    int32_t i;
    for(i = 0; i < n; i++) {
        wchar_t c = i_get_char(port);
        if (c == -1) {
            break;
        }
        chars[i] = c;
    }
    chars[i] = 0;
    return i == 0 ? S_EOF : string2scm(wstring(chars, n));
}

SchemeObject* s_get_string_all(Scheme* scheme, SchemeObject* port) {
    assert_arg_input_port_type(L"get-string-all", 1, port);
    assert_arg_textual_port_type(L"get-string-all", 1, port);
    wstring str = L"";
    
    int32_t i = 0;
    while(true) {
        wchar_t c = i_get_char(port);
        if (c == -1) {
            break;
        } else {
            i++;
            str += c;
        }
    }
    return i == 0 ? S_EOF : string2scm(str);
}

SchemeObject* s_get_line(Scheme* scheme, SchemeObject* port) {
    assert_arg_input_port_type(L"get-line", 1, port);
    assert_arg_textual_port_type(L"get-line", 1, port);
    wstring str = L"";
    
    int32_t i = 0;
    while(true) {
        wchar_t c = i_get_char(port);
        if (c == -1) {
            break;
        } else if (c == L'\n') {
            i++;
            break;
        } else {
            i++;
            str += c;
        }
    }
    return i == 0 ? S_EOF : string2scm(str);
}


void R6RSLibIOPorts::bind(Scheme* scheme, SchemeObject* envt) {
    
    latin1_codec = SchemeObject::createCodec(new Latin1Codec());
    utf8_codec = SchemeObject::createCodec(new UTF8Codec());
    utf16_codec = SchemeObject::createCodec(new UTF16Codec());
    rawunicode_codec = SchemeObject::createCodec(new RawUnicodeCodec());
    
    scheme->keepForever(latin1_codec);
    scheme->keepForever(utf8_codec);
    scheme->keepForever(utf16_codec);
    scheme->keepForever(rawunicode_codec);
    
	scheme->assign(L"latin-1-codec"         ,0,0,0, (SchemeObject* (*)()) s_latin_1_codec, envt);
	scheme->assign(L"utf-8-codec"           ,0,0,0, (SchemeObject* (*)()) s_utf_8_codec, envt);
	scheme->assign(L"utf-16-codec"          ,0,0,0, (SchemeObject* (*)()) s_utf_16_codec, envt);
	scheme->assign(L"native-eol-style"      ,0,0,0, (SchemeObject* (*)()) s_native_eol_style, envt);
	scheme->assign(L"make-transcoder"       ,1,2,0, (SchemeObject* (*)()) s_make_transcoder, envt);
	scheme->assign(L"native-transcoder"     ,0,0,0, (SchemeObject* (*)()) s_native_transcoder, envt);
	scheme->assign(L"transcoder-codec"      ,1,0,0, (SchemeObject* (*)()) s_transcoder_codec, envt);
	scheme->assign(L"transcoder-eol-style"  ,1,0,0, (SchemeObject* (*)()) s_transcoder_eol_style, envt);
	scheme->assign(L"transcoder-error-handling-mode"
	                                        ,1,0,0, (SchemeObject* (*)()) s_transcoder_error_handling_mode, envt);
	scheme->assign(L"transcoder-codec"      ,1,0,0, (SchemeObject* (*)()) s_transcoder_codec, envt);
	scheme->assign(L"port?"                 ,1,0,0, (SchemeObject* (*)()) s_port_p, envt);
	scheme->assign(L"port-transcoder"       ,1,0,0, (SchemeObject* (*)()) s_port_transcoder, envt);
	scheme->assign(L"textual-port?"         ,1,0,0, (SchemeObject* (*)()) s_textual_port_p, envt);
	scheme->assign(L"binary-port?"          ,1,0,0, (SchemeObject* (*)()) s_binary_port_p, envt);
	scheme->assign(L"transcoded-port"       ,2,0,0, (SchemeObject* (*)()) s_transcoded_port, envt);
	scheme->assign(L"port-eof?"             ,1,0,0, (SchemeObject* (*)()) s_port_eof_p, envt);
	scheme->assign(L"open-file-input-port"  ,1,3,0, (SchemeObject* (*)()) s_open_file_input_port, envt);
	scheme->assign(L"open-bytevector-input-port"
	                                        ,1,1,0, (SchemeObject* (*)()) s_open_bytevector_input_port, envt);
	scheme->assign(L"open-string-input-port",1,1,0, (SchemeObject* (*)()) s_open_string_input_port, envt);
	scheme->assign(L"get-u8"                ,1,0,0, (SchemeObject* (*)()) s_get_u8, envt);
	scheme->assign(L"lookahead-u8"          ,1,0,0, (SchemeObject* (*)()) s_lookahead_u8, envt);
	scheme->assign(L"get-char"              ,1,0,0, (SchemeObject* (*)()) s_get_char, envt);
	scheme->assign(L"lookahead-char"        ,1,0,0, (SchemeObject* (*)()) s_lookahead_char, envt);
	scheme->assign(L"get-string-n"          ,2,0,0, (SchemeObject* (*)()) s_get_string_n, envt);
	scheme->assign(L"get-string-all"        ,1,0,0, (SchemeObject* (*)()) s_get_string_all, envt);
	scheme->assign(L"get-line"              ,1,0,0, (SchemeObject* (*)()) s_get_line, envt);

    // Defined in io-common
	scheme->assign(L"current-input-port"    ,0,0,0, (SchemeObject* (*)()) s_current_input_port, envt);
	scheme->assign(L"current-output-port"   ,0,0,0, (SchemeObject* (*)()) s_current_output_port, envt);
	scheme->assign(L"input-port?"           ,1,0,0, (SchemeObject* (*)()) s_input_port_p, envt);
	scheme->assign(L"output-port?"          ,1,0,0, (SchemeObject* (*)()) s_output_port_p, envt);
	scheme->assign(L"eof-object?"           ,1,0,0, (SchemeObject* (*)()) s_eof_object_p, envt);
}

wchar_t Codec::peek(SchemeObject* port) {
    wchar_t c = this->get(port);
    this->unget(port);
    return c;
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

wchar_t UTF8Codec::get(SchemeObject* port) {
    istream* is = port->is;
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

void UTF8Codec::unget(SchemeObject* port) {
    istream* is = port->is;
    int32_t c;
    do {
        is->unget();
        c = is->peek();
    } while (c >= 128 && (c & LEN2_VAL) != LEN2_VAL);
    is->clear();
}

wchar_t UTF16Codec::get(SchemeObject* port) {
    istream* is = port->is;
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

void UTF16Codec::unget(SchemeObject* port) {
    port->is->unget();
    port->is->unget();
    port->is->clear();
}

wchar_t Latin1Codec::get(SchemeObject* port) {
    int32_t c = port->is->get();
    return c;
}

void Latin1Codec::unget(SchemeObject* port) {
    port->is->unget();
    port->is->clear();
}


wchar_t RawUnicodeCodec::get(SchemeObject* port) {
    return port->wis->get();
}

void RawUnicodeCodec::unget(SchemeObject* port) {
    port->wis->unget();
    port->wis->clear();
}
