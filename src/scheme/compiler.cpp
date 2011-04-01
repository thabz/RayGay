
#include "scheme.h"
#include "numbers.h"
#include "compiler.h"
#include "objects.h"

#include <cstdio>
#include <cstring>

void compile_form(SchemeObject* form) {
    SchemeObject::ObjectType t = form->type();
    if (t == SchemeObject::EMPTY_LIST) {
    } else if (t == SchemeObject::PAIR) {
	compile_form(i_car(form));
	compile_form(i_cdr(form));
    } else if (t == SchemeObject::INTEGER_NUMBER) {
	wcout << L"Integer: " << form->integer_value << endl;
    } else if (t == SchemeObject::SYMBOL) {
	wcout << L"Symbol: " << form->toString() << endl;
    } else {
        wcout << L"Unsupported form: " << form->toString(t) << endl;
    }
}

// Returns the tokenized source code of a userProcedure
SchemeObject* s_source(Scheme* scheme, SchemeObject* userProcedure) {
    assert(userProcedure->type() == SchemeObject::USER_PROCEDURE);
    SchemeObject* body = i_cadr(userProcedure->s_closure_data);
    return body;
}

SchemeObject* s_compile(Scheme* scheme, SchemeObject* userProcedure) {
    void *code = NULL;
    SchemeObject* body = i_cadr(userProcedure->s_closure_data);
    compile_form(body);

    return SchemeObject::createCompiledProcedure(userProcedure, code);
}

SchemeObject* s_call_compiled_code(Scheme* scheme, SchemeObject* compiled_function, SchemeObject* args) {
    return NULL;
}

void Compiler::bind(Scheme* scheme, SchemeObject* envt) {
//    scheme->assign(L"$compile"         ,1,0,0, (SchemeObject* (*)()) s_compile, envt);
    scheme->assign(L"$source"         ,1,0,0, (SchemeObject* (*)()) s_source, envt);
}

