
#ifndef R6RS_LIB_NUMBERS_H
#define R6RS_LIB_NUMBERS_H

#include "scheme.h"

struct LibNumbers {
    static void bind(Scheme* scheme, SchemeObject* envt);
};

SchemeObject* i_string_2_number(wstring s, uint32_t radix, size_t offset = 0);
wstring i_number_2_string(SchemeObject* o, uint32_t radix);

// Math stuff
SchemeObject* s_equal(int num, SchemeStack::iterator stack);
SchemeObject* s_less(int num, SchemeStack::iterator stack);
SchemeObject* s_greater(int num, SchemeStack::iterator stack);
SchemeObject* s_less_equal(int num, SchemeStack::iterator stack);
SchemeObject* s_greater_equal(int num, SchemeStack::iterator stack);
SchemeObject* s_plus(int num, SchemeStack::iterator stack);
SchemeObject* s_minus(int num, SchemeStack::iterator stack);
SchemeObject* s_mult(int num, SchemeStack::iterator stack);
SchemeObject* s_divide(int num, SchemeStack::iterator stack);
SchemeObject* s_sqrt(SchemeObject* n);
SchemeObject* s_abs(SchemeObject* n);
SchemeObject* s_sin(SchemeObject* n);
SchemeObject* s_cos(SchemeObject* n);
SchemeObject* s_asin(SchemeObject* n);
SchemeObject* s_acos(SchemeObject* n);
SchemeObject* s_tan(SchemeObject* n);
SchemeObject* s_atan(SchemeObject* y, SchemeObject* x);
SchemeObject* s_expt(SchemeObject* y, SchemeObject* x);
SchemeObject* s_exp(SchemeObject* n);
SchemeObject* s_log(SchemeObject* n);
SchemeObject* s_min(int num, SchemeStack::iterator stack);
SchemeObject* s_max(int num, SchemeStack::iterator stack);
SchemeObject* s_gcd(int num, SchemeStack::iterator stack);
SchemeObject* s_lcm(int num, SchemeStack::iterator stack);
SchemeObject* s_numerator(SchemeObject* n);
SchemeObject* s_denominator(SchemeObject* n);
SchemeObject* s_round(SchemeObject* n);
SchemeObject* s_floor(SchemeObject* n);
SchemeObject* s_ceiling(SchemeObject* n);
SchemeObject* s_truncate(SchemeObject* n);
SchemeObject* s_even_p(SchemeObject* n);
SchemeObject* s_odd_p(SchemeObject* n);
SchemeObject* s_zero_p(SchemeObject* n);
SchemeObject* s_negative_p(SchemeObject* n);
SchemeObject* s_positive_p(SchemeObject* n);
SchemeObject* s_integer_p(SchemeObject* n);
SchemeObject* s_complex_p(SchemeObject* n);
SchemeObject* s_rational_p(SchemeObject* n);
SchemeObject* s_real_p(SchemeObject* n);
SchemeObject* s_exact_p(SchemeObject* n);
SchemeObject* s_inexact_p(SchemeObject* n);
SchemeObject* s_exact_2_inexact(SchemeObject* n);
SchemeObject* s_inexact_2_exact(SchemeObject* n);
SchemeObject* s_quotient(SchemeObject* n1, SchemeObject* n2);
SchemeObject* s_remainder(SchemeObject* n1, SchemeObject* n2);
SchemeObject* s_modulo(SchemeObject* n1, SchemeObject* n2);
SchemeObject* s_make_polar(SchemeObject* magnitude, SchemeObject* angle);
SchemeObject* s_make_rectangular(SchemeObject* real, SchemeObject* imag);
SchemeObject* s_real_part(SchemeObject* z);
SchemeObject* s_imag_part(SchemeObject* z);
SchemeObject* s_magnitude(SchemeObject* z);
SchemeObject* s_angle(SchemeObject* z);
SchemeObject* s_number_p(SchemeObject* p);

#endif

