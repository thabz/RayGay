
#include "compiler.h"
#include "numbers.h"
#include "objects.h"
#include "scheme.h"

#include <cstdio>
#include <cstring>

// Returns the tokenized source code of a userProcedure
SchemeObject *s_source(Scheme *scheme, SchemeObject *userProcedure) {
  assert(userProcedure->type() == SchemeObject::USER_PROCEDURE);
  SchemeObject *body = i_cadr(userProcedure->s_closure_data);
  return body;
}

SchemeObject *s_make_native_procedure(Scheme *scheme, SchemeObject *bytevector,
                                      SchemeObject *originalUserProcedure) {
  assert(originalUserProcedure->type() == SchemeObject::USER_PROCEDURE);
  return SchemeObject::createCompiledProcedure(
      originalUserProcedure, bytevector->bytevector, bytevector->length);
}

SchemeObject *s_call_compiled_code(Scheme *scheme,
                                   SchemeObject *compiled_function,
                                   SchemeObject *args) {
  return NULL;
}

void Compiler::bind(Scheme *scheme, SchemeObject *envt) {
  scheme->assign(L"$make-native-procedure", 2, 0, 0,
                 (SchemeObject * (*)()) s_make_native_procedure, envt);
  scheme->assign(L"$source", 1, 0, 0, (SchemeObject * (*)()) s_source, envt);
}
