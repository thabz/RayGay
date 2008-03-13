
#include "r6rs-lib-io-simple.h"
#include "r6rs-lib-io-common.h"
#include <fstream>
#include "filenames.h"
#include "parser.h"

SchemeObject* s_open_input_file(Scheme* scheme, SchemeObject* s_filename) {
    assert_arg_string_type(L"open-input-file", 1, s_filename);
    wifstream* wifs = new wifstream(SchemeFilenames::toFilename(scm2string(s_filename)).c_str(), ios::in);

    try {
        wifs->imbue(locale(""));
    } catch (std::runtime_error e) {
        wcout << L"Warning: can't read system locale. Any UTF-8 data in " << scm2string(s_filename) << L" won't be read correctly." << endl;
    }

    if (wifs->fail()) {
        throw scheme_exception(L"Error opening file " + s_filename->toString());
    }
    return SchemeObject::createInputPort(wifs);
}

SchemeObject* s_open_output_file(Scheme* scheme, SchemeObject* s_filename) {
    assert_arg_string_type(L"open-output-file", 1, s_filename);
    wofstream* wofs = new wofstream(SchemeFilenames::toFilename(scm2string(s_filename)).c_str(), ios::out);
    try {
        wofs->imbue(locale(""));
    } catch (std::runtime_error e) {
        wcout << L"Warning: can't read system locale. Any UTF-8 written to " << scm2string(s_filename) << L" won't work." << endl;
    }
    
    if (wofs->fail()) {
        throw scheme_exception(L"Error opening file " + s_filename->toString() + L" for writing.");
    }
    return SchemeObject::createOutputPort(wofs);
}

SchemeObject* s_close_input_port(Scheme* scheme, SchemeObject* s_port) {
    assert_arg_type(scheme,L"close-input-port", 1, s_input_port_p, s_port);
    wistream* wis = s_port->wis;
    // Only file-streams can be closed in C++
    wifstream* wifs = static_cast<wifstream*>(wis);
    if (wifs != NULL) {
       wifs->close();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_close_output_port(Scheme* scheme, SchemeObject* s_port) {
    assert_arg_type(scheme,L"close-output-port", 1, s_output_port_p, s_port);
    wostream* wos = s_port->wos;
    // Only file-streams can be closed in C++
    wofstream* wofs = static_cast<wofstream*>(wos);
    if (wofs != NULL) {
       wofs->close();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_call_with_input_file(Scheme* scheme, SchemeObject* s_filename, SchemeObject* s_proc) {
    assert_arg_string_type(L"with-input-file", 1, s_filename);
    assert_arg_procedure_type(L"with-input-file", 2, s_proc);
    SchemeObject* input_port = s_open_input_file(scheme, s_filename);
    SchemeObject* result = scheme->callProcedure_1(s_proc, input_port);
    s_close_input_port(scheme,input_port);
    return result;
}

SchemeObject* s_call_with_output_file(Scheme* scheme, SchemeObject* s_filename, SchemeObject* s_proc) {
    assert_arg_string_type(L"with-output-file", 1, s_filename);
    assert_arg_procedure_type(L"with-output-file", 2, s_proc);
    SchemeObject* output_port = s_open_output_file(scheme, s_filename);
    SchemeObject* result = scheme->callProcedure_1(s_proc, output_port);
    s_close_output_port(scheme,output_port);
    return result;
}

SchemeObject* s_with_output_to_file(Scheme* scheme, SchemeObject* s_filename, SchemeObject* s_thunk) {
    assert_arg_string_type(L"with-input-to-file", 1, s_filename);
    assert_arg_procedure_type(L"with-input-to-file", 2, s_thunk);
    SchemeObject* saved_output_port = scheme->current_output_port;
    scheme->current_output_port = s_open_output_file(scheme, s_filename);
    SchemeObject* result = scheme->callProcedure_0(s_thunk);
    s_close_output_port(scheme, scheme->current_output_port);
    scheme->current_output_port = saved_output_port;
    return result;
}

SchemeObject* s_with_input_from_file(Scheme* scheme, SchemeObject* s_filename, SchemeObject* s_thunk) {
    assert_arg_string_type(L"with-input-from-file", 1, s_filename);
    assert_arg_procedure_type(L"with-input-from-file", 2, s_thunk);
    SchemeObject* saved_input_port = scheme->current_input_port;
    scheme->current_input_port = s_open_input_file(scheme, s_filename);
    SchemeObject* result = scheme->callProcedure_0(s_thunk);
    s_close_input_port(scheme, scheme->current_input_port);
    scheme->current_input_port = saved_input_port;
    return result;
}

SchemeObject* s_read_char(Scheme* scheme, SchemeObject* s_port) {
    wistream* wis;
    if (s_port == S_UNSPECIFIED) {
        wis = scheme->current_input_port->wis;
    } else {
        assert_arg_type(scheme, L"read-char", 1, s_input_port_p, s_port);
        wis = s_port->wis;
    }
    int c = wis->get();
    if (c == -1) {
        return S_EOF;
    } else {
        return char2scm(c);
    }
}

SchemeObject* s_peek_char(Scheme* scheme, SchemeObject* s_port) {
    wistream* wis;
    if (s_port == S_UNSPECIFIED) {
        wis = s_current_input_port(scheme)->wis;
    } else {
        assert_arg_type(scheme, L"peek-char", 1, s_input_port_p, s_port);
        wis = s_port->wis;
    }
    int c = wis->peek();
    if (c == -1) {
        return S_EOF;
    } else {
        return char2scm(c);
    }
}

SchemeObject* s_write_char(Scheme* scheme, SchemeObject* s_char, SchemeObject* port) {
    assert_arg_char_type(L"write-char", 1, s_char);
    wostream* wos;
    if (port == S_UNSPECIFIED) {
        wos = s_current_output_port(scheme)->wos;
    } else {
        assert_arg_type(scheme, L"write-char", 2, s_output_port_p, port);
        wos = port->wos;
    }
    (*wos) << scm2char(s_char);
    return S_UNSPECIFIED;
}

SchemeObject* s_read(Scheme* scheme, SchemeObject* s_port) {
    if (s_port == S_UNSPECIFIED) {
        s_port = s_current_input_port(scheme);
    } else {
        assert_arg_type(scheme, L"read", 1, s_input_port_p, s_port);
    }
    return scheme->getParser()->read(s_port);
}


SchemeObject* s_write(Scheme* scheme, SchemeObject* o, SchemeObject* port) {
    wostream* wos;
    if (port == S_UNSPECIFIED) {
        wos = s_current_output_port(scheme)->wos;
    } else {
        assert_arg_type(scheme, L"write", 2, s_output_port_p, port);
        wos = port->wos;
    }
    (*wos) << o->toString();
    return S_UNSPECIFIED;
}

SchemeObject* s_display(Scheme* scheme, SchemeObject* o, SchemeObject* port) {
    wostream* wos;
    if (port == S_UNSPECIFIED) {
        wos = s_current_output_port(scheme)->wos;
    } else {
        assert_arg_type(scheme, L"display", 2, s_output_port_p, port);
        wos = port->wos;
    }
    
    if (i_string_p(o) == S_TRUE) {
        (*wos) << o->str;
    } else if (i_char_p(o) == S_TRUE) {
        (*wos) << o->c;
    } else {
        (*wos) << o->toString();
    }
    return S_UNSPECIFIED;
}

SchemeObject* s_newline(Scheme* scheme, SchemeObject* port) {
    wostream* wos;
    if (port == S_UNSPECIFIED) {
        wos = s_current_output_port(scheme)->wos;
    } else {
        assert_arg_type(scheme, L"write", 2, s_output_port_p, port);
        wos = port->wos;
    }
    (*wos) << endl;        
    return S_UNSPECIFIED;
}

void R6RSLibIOSimple::bind(Scheme* scheme, SchemeObject* envt) {
    // Defined in io-common
	scheme->assign(L"current-input-port"    ,0,0,0, (SchemeObject* (*)()) s_current_input_port, envt);
	scheme->assign(L"current-output-port"   ,0,0,0, (SchemeObject* (*)()) s_current_output_port, envt);
	scheme->assign(L"input-port?"           ,1,0,0, (SchemeObject* (*)()) s_input_port_p, envt);
	scheme->assign(L"output-port?"          ,1,0,0, (SchemeObject* (*)()) s_output_port_p, envt);
	scheme->assign(L"eof-object?"           ,1,0,0, (SchemeObject* (*)()) s_eof_object_p, envt);
    
    // Defined here
	scheme->assign(L"call-with-input-file"  ,2,0,0, (SchemeObject* (*)()) s_call_with_input_file, envt);
	scheme->assign(L"call-with-output-file" ,2,0,0, (SchemeObject* (*)()) s_call_with_output_file, envt);
	scheme->assign(L"with-input-from-file"  ,2,0,0, (SchemeObject* (*)()) s_with_input_from_file, envt);
	scheme->assign(L"with-output-to-file"   ,2,0,0, (SchemeObject* (*)()) s_with_output_to_file, envt);
	scheme->assign(L"open-input-file"       ,1,0,0, (SchemeObject* (*)()) s_open_input_file, envt);
	scheme->assign(L"open-output-file"      ,1,0,0, (SchemeObject* (*)()) s_open_output_file, envt);
	scheme->assign(L"close-input-port"      ,1,0,0, (SchemeObject* (*)()) s_close_input_port, envt);
	scheme->assign(L"close-output-port"     ,1,0,0, (SchemeObject* (*)()) s_close_output_port, envt);
	scheme->assign(L"read-char"             ,0,1,0, (SchemeObject* (*)()) s_read_char, envt);
	scheme->assign(L"peek-char"             ,0,1,0, (SchemeObject* (*)()) s_peek_char, envt);
	scheme->assign(L"write-char"            ,1,1,0, (SchemeObject* (*)()) s_write_char, envt);
	scheme->assign(L"read"                  ,0,1,0, (SchemeObject* (*)()) s_read, envt);

}
