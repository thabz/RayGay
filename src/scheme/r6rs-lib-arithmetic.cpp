
#include "r6rs-lib-arithmetic.h"
#include "scheme.h"
#include "numbers.h"

SchemeObject* s_bitwise_or(int num, SchemeStack::iterator stack) {
    if (num == 0) {
	return S_ZERO;
    }

    assert_arg_int_type(L"bitwise-or", 1, *stack);
    int64_t result = scm2int(*stack++);
    int i = 1;
    while (i++ != num) {
        assert_arg_int_type(L"bitwise-or", i, *stack);
	result |= (scm2int(*stack++));
	i++;
    }
    return int2scm(result);
}

SchemeObject* s_bitwise_and(int num, SchemeStack::iterator stack) {
    if (num == 0) {
	return int2scm(-1);
    }

    assert_arg_int_type(L"bitwise-and", 1, *stack);
    int64_t result = scm2int(*stack++);
    int i = 1;
    while (i++ != num) {
        assert_arg_int_type(L"bitwise-and", i, *stack);
	result &= (scm2int(*stack++));
    }
    return int2scm(result);
}

SchemeObject* s_bitwise_xor(int num, SchemeStack::iterator stack) {
    if (num == 0) {
	return S_ZERO;
    }

    assert_arg_int_type(L"bitwise-and", 1, *stack);
    int64_t result = scm2int(*stack++);
    int i = 0;
    while (i++ != num) {
    assert_arg_int_type(L"bitwise-xor", i, *stack);
	result ^= (scm2int(*stack++));
    }
    return int2scm(result);
}

SchemeObject* s_bitwise_ior(int num, SchemeStack::iterator stack) {
    throw scheme_exception(L"Not implemented");
}

SchemeObject* s_bitwise_not(SchemeObject* ei) {
    assert_arg_int_type(L"bitwise-not", 1, ei);
    int64_t result = scm2int(ei);
    return int2scm(~result);
}

SchemeObject* s_bitwise_if(SchemeObject* ei1, SchemeObject* ei2, SchemeObject* ei3) {
    throw scheme_exception(L"Not implemented");
//    return s_bitwise_ior(s_bitwise_and(ei1,ei2), 
//	                 s_bitwise_and(s_bitwise_not(ei1), ei3))
}

void R6RSLibArithmetic::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign(L"bitwise-and"     ,0,0,1, (SchemeObject* (*)()) s_bitwise_and, envt);
    scheme->assign(L"bitwise-or"      ,0,0,1, (SchemeObject* (*)()) s_bitwise_or, envt);
    scheme->assign(L"bitwise-xor"     ,0,0,1, (SchemeObject* (*)()) s_bitwise_xor, envt);
    scheme->assign(L"bitwise-ior"     ,0,0,1, (SchemeObject* (*)()) s_bitwise_ior, envt);
    scheme->assign(L"bitwise-not"     ,1,0,0, (SchemeObject* (*)()) s_bitwise_not, envt);
    scheme->assign(L"bitwise-if"      ,0,0,1, (SchemeObject* (*)()) s_bitwise_if, envt);
}
