
#include "r6rs-lib-arithmetic.h"
#include "scheme.h"

SchemeObject* s_bitwise_or(int num, SchemeStack::iterator stack) {
    if (num == 0) {
	return S_ZERO;
    }

    int64_t result = scm2int(*stack++);
    num--;
    while (num-- != 0) {
	result |= (scm2int(*stack++));
    }
    return int2scm(result);
}

SchemeObject* s_bitwise_and(int num, SchemeStack::iterator stack) {
    if (num == 0) {
	return int2scm(-1);
    }

    int64_t result = scm2int(*stack++);
    num--;
    while (num-- != 0) {
	result &= (scm2int(*stack++));
    }
    return int2scm(result);
}

SchemeObject* s_bitwise_xor(int num, SchemeStack::iterator stack) {
    if (num == 0) {
	return S_ZERO;
    }

    int64_t result = scm2int(*stack++);
    num--;
    while (num-- != 0) {
	result ^= (scm2int(*stack++));
    }
    return int2scm(result);
}

void R6RSLibArithmetic::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign(L"bitwise-and"     ,0,0,1, (SchemeObject* (*)()) s_bitwise_and, envt);
    scheme->assign(L"bitwise-or"      ,0,0,1, (SchemeObject* (*)()) s_bitwise_or, envt);
    scheme->assign(L"bitwise-xor"     ,0,0,1, (SchemeObject* (*)()) s_bitwise_xor, envt);
}
