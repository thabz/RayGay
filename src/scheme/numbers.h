
#ifndef R6RS_LIB_NUMBERS_H
#define R6RS_LIB_NUMBERS_H

#include "scheme.h"

struct LibNumbers {
  static void bind(Scheme *scheme, SchemeObject *envt);
};

SchemeObject *i_string_2_number(Scheme *, wstring s, uint32_t radix,
                                size_t offset = 0);
wstring i_number_2_string(SchemeObject *o, uint32_t radix);

SchemeObject *i_integer_p(SchemeObject *);
SchemeObject *i_complex_p(SchemeObject *);
SchemeObject *i_real_p(SchemeObject *);

// Math stuff
SchemeObject *s_equal(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_less(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_greater(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_less_equal(Scheme *scheme, int num,
                           SchemeStack::iterator stack);
SchemeObject *s_greater_equal(Scheme *scheme, int num,
                              SchemeStack::iterator stack);
SchemeObject *s_plus(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_minus(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_mult(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_divide(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_sqrt(Scheme *scheme, SchemeObject *n);
SchemeObject *s_abs(Scheme *scheme, SchemeObject *n);
SchemeObject *s_sin(Scheme *scheme, SchemeObject *n);
SchemeObject *s_cos(Scheme *scheme, SchemeObject *n);
SchemeObject *s_asin(Scheme *scheme, SchemeObject *n);
SchemeObject *s_acos(Scheme *scheme, SchemeObject *n);
SchemeObject *s_tan(Scheme *scheme, SchemeObject *n);
SchemeObject *s_atan(Scheme *scheme, SchemeObject *y, SchemeObject *x);
SchemeObject *s_expt(Scheme *scheme, SchemeObject *y, SchemeObject *x);
SchemeObject *s_exp(Scheme *scheme, SchemeObject *n);
SchemeObject *s_log(Scheme *scheme, SchemeObject *n);
SchemeObject *s_min(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_max(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_gcd(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_lcm(Scheme *scheme, int num, SchemeStack::iterator stack);
SchemeObject *s_numerator(Scheme *scheme, SchemeObject *n);
SchemeObject *s_denominator(Scheme *scheme, SchemeObject *n);
SchemeObject *s_round(Scheme *scheme, SchemeObject *n);
SchemeObject *s_floor(Scheme *scheme, SchemeObject *n);
SchemeObject *s_ceiling(Scheme *scheme, SchemeObject *n);
SchemeObject *s_truncate(Scheme *scheme, SchemeObject *n);
SchemeObject *s_even_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_odd_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_zero_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_negative_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_positive_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_finite_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_infinite_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_integer_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_complex_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_rational_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_real_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_exact_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_inexact_p(Scheme *scheme, SchemeObject *n);
SchemeObject *s_exact_2_inexact(Scheme *scheme, SchemeObject *n);
SchemeObject *s_inexact_2_exact(Scheme *scheme, SchemeObject *n);
SchemeObject *s_quotient(Scheme *scheme, SchemeObject *n1, SchemeObject *n2);
SchemeObject *s_remainder(Scheme *scheme, SchemeObject *n1, SchemeObject *n2);
SchemeObject *s_modulo(Scheme *scheme, SchemeObject *n1, SchemeObject *n2);
SchemeObject *s_make_polar(Scheme *scheme, SchemeObject *magnitude,
                           SchemeObject *angle);
SchemeObject *s_make_rectangular(Scheme *scheme, SchemeObject *real,
                                 SchemeObject *imag);
SchemeObject *s_real_part(Scheme *scheme, SchemeObject *z);
SchemeObject *s_imag_part(Scheme *scheme, SchemeObject *z);
SchemeObject *s_magnitude(Scheme *scheme, SchemeObject *z);
SchemeObject *s_angle(Scheme *scheme, SchemeObject *z);
SchemeObject *s_number_p(Scheme *scheme, SchemeObject *p);

#endif
