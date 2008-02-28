
#include "r6rs-lib-bytevectors.h"

SchemeObject* s_bytevector_p(SchemeObject* o) {
    return bool2scm(i_bytevector_p(o));
}

SchemeObject* s_string_2_utf8(SchemeObject* str) {
    return S_FALSE;
}

SchemeObject* s_utf8_2_string(SchemeObject* bytevector) {
    return S_FALSE;
}

void R6RSLibBytevectors::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign(L"bytevector?"           ,1,0,0, (SchemeObject* (*)()) s_bytevector_p, envt);
    scheme->assign(L"string->utf8"          ,1,0,0, (SchemeObject* (*)()) s_string_2_utf8, envt);
    scheme->assign(L"utf8->string"          ,1,0,0, (SchemeObject* (*)()) s_utf8_2_string, envt);
}
