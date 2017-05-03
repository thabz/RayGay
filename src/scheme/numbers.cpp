
#include "numbers.h"

#include <cmath>
#include <cerrno>
#include <iomanip>
#include <cstdlib>
#include <cstring>

void coerceNumbers(int num, SchemeStack::iterator stack, double* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2double(*stack);
        stack++;
        dest++;
    }
}

void coerceNumbers(int num, SchemeStack::iterator stack, int64_t* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2int(*stack);
        stack++;
        dest++;
    }
}

void coerceNumbers(int num, SchemeStack::iterator stack, rational_type* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2rational(*stack);
        stack++;
        dest++;
    }
}

void coerceNumbers(int num, SchemeStack::iterator stack, std::complex<double>* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2complex(*stack);
        stack++;
        dest++;
    }
}

// This returns the best number type for the numbers in stacks. Ie. if the 
// stack contains all integers but just one rational, the rational is 
// returned. If it's all rationals but with just one real, the real-type 
// is returned. 
SchemeObject::ObjectType representativeNumberType(const wchar_t* procname, int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType type = SchemeObject::INTEGER_NUMBER;
    for(int i = 0; i < num; i++) {
        SchemeObject* n = *stack++;
        SchemeObject::ObjectType t = n->type();
        if (t < SchemeObject::NUMBERS_ARE_BEFORE_HERE) {
            if (t < type) type = t;
        } else {
            wrong_type_arg(procname, i+1, n);
        }
    }
    return type;
}

SchemeObject* s_plus(Scheme* scheme, int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"+", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
       double* args = (double*)::alloca(sizeof(double)*num);
       coerceNumbers(num,stack,args);
       
       double result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
       coerceNumbers(num,stack,args);
       
       int64_t result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double>* args = (std::complex<double>*)::alloca(sizeof(std::complex<double>)*num);
       coerceNumbers(num,stack,args);
       
       std::complex<double> result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return complex2scm(result);
    } else {
       rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
       coerceNumbers(num,stack,args);
       
       rational_type result(0);
       for(int i = 0; i < num; i++) {
	       result += args[i];
       }
       return rational2scm(result);
    }
}

SchemeObject* s_minus(Scheme* scheme, int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"-", num, stack);
    
    if (outType == SchemeObject::REAL_NUMBER) {
       double* args = (double*)::alloca(sizeof(double)*num);
       coerceNumbers(num,stack,args);
       double result = args[0];
       if (num == 1) {
           // One-argument case is a simple negate (n => -n)
           return double2scm(-result);
       } 
       for(int i = 1; i < num; i++) {
           result -= args[i];    
       }
       return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
       coerceNumbers(num,stack,args);
       int64_t result = args[0];
       if (num == 1) {
           // One-argument case is a simple negate (n => -n)
           return int2scm(-result);
       } 
       for(int i = 1; i < num; i++) {
           result -= args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double>* args = (std::complex<double>*)::alloca(sizeof(std::complex<double>)*num);
       coerceNumbers(num,stack,args);
       std::complex<double> result = args[0];
       if (num == 1) {
           // One-argument case is a simple negate (n => -n)
           return complex2scm(-result);
       } 
       for(int i = 1; i < num; i++) {
           result -= args[i];    
       }
       return complex2scm(result);
    } else {
       rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
       coerceNumbers(num,stack,args);
       
       rational_type result = args[0];
       if (num == 1) {
           return rational2scm(-result);
       }
       
       // Using a/b - c/d = (ad - bc)/bd
       for(int i = 1; i < num; i++) {
	       result -= args[i];
       }
       return rational2scm(result);
    }        
}

SchemeObject* s_divide(Scheme* scheme, int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"/", num, stack);
    if (outType == SchemeObject::REAL_NUMBER) {
       double* args = (double*)::alloca(sizeof(double)*num);
       coerceNumbers(num,stack,args);
       double result = args[0];
       if (num == 1) {
           // One-argument case is a simple inverse (n => 1/n)
           return double2scm(1.0 / result);
       } 
       for(int i = 1; i < num; i++) {
           result /= args[i];    
       }
       return double2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double>* args = (std::complex<double>*)::alloca(sizeof(std::complex<double>)*num);
       coerceNumbers(num,stack,args);
       std::complex<double> result = args[0];
       if (num == 1) {
           // One-argument case is a simple inverse (n => 1/n)
           return complex2scm(1.0 / result);
       } 
       for(int i = 1; i < num; i++) {
           result /= args[i];    
       }
       return complex2scm(result);
    } else {
       // Both integers and rational types are handled as rationals            
       rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
       coerceNumbers(num,stack,args);

       rational_type result = args[0];
       if (num == 1) {
           // One-argument case is a simple inverse (n => 1/n)
           return rational2scm(result.inverse());
       }

       // Using a/b / c/d = ad/bc
       for(int i = 1; i < num; i++) {
	   result /= args[i];
       }
       return rational2scm(result);
   }
}

SchemeObject* s_mult(Scheme* scheme, int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"*", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
       double* args = (double*)::alloca(sizeof(double)*num);
       coerceNumbers(num,stack,args);
       
       double result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
       coerceNumbers(num,stack,args);
       
       int64_t result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double>* args = (std::complex<double>*)::alloca(sizeof(std::complex<double>)*num);
       coerceNumbers(num,stack,args);
       
       std::complex<double> result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return complex2scm(result);
    } else {
       rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
       coerceNumbers(num,stack,args);
       
       rational_type result(1);
       for(int i = 0; i < num; i++) {
	   result *= args[i]; 
       }
       return rational2scm(result);
    }
}


SchemeObject* s_sqrt(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"sqrt", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::sqrt(scm2complex(n)));
    } else {
        double d = scm2double(n);
        if (d < 0) {
            return complex2scm(std::sqrt(scm2complex(n)));
        } else {
            return double2scm(sqrt(scm2double(n)));
        }
    }
}

// See http://en.wikipedia.org/wiki/Integer_square_root
SchemeObject* s_exact_integer_sqrt(Scheme* scheme, SchemeObject* s_n) {
    assert_arg_non_negative_int(L"exact-integer-sqrt", 1, s_n);
    int64_t n = scm2int(s_n);
    if (n == 0) {
        return i_list_2(int2scm(0),int2scm(0));
    }
    int64_t xn = n;
    int64_t x;
    do {
        x = xn;
        xn = (x + (n / x)) / 2;
    } while (abs(x-xn) > 0);
    return i_list_2(int2scm(xn), int2scm(n-xn*xn));
}

SchemeObject* s_abs(Scheme* scheme, SchemeObject* n) {
    assert_arg_real_type(L"abs", 1, n);
    if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        return rational2scm(abs(scm2rational(n)));
    } else if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return int2scm(labs(scm2int(n)));
    } else {
        return double2scm(fabs(scm2double(n)));
    }         
}


SchemeObject* s_sin(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"sin", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::sin(scm2complex(n)));
    } else {
        return double2scm(::sin(scm2double(n)));
    }
}

SchemeObject* s_asin(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"asin", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        complex<double> i = complex<double>(0,1);    
        complex<double> z = scm2complex(n);
        // Using the formula from R^5RS
        return complex2scm(-i * std::log(i*z + std::sqrt(1.0-z*z)));     
    } else {
        return double2scm(::asin(scm2double(n)));
    }
}

SchemeObject* s_cos(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"cos", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::cos(scm2complex(n)));
    } else {
        return double2scm(::cos(scm2double(n)));
    }
}

SchemeObject* s_acos(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"acos", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        complex<double> i = complex<double>(0,1);    
        complex<double> z = scm2complex(n);
        // Using the formula from R^5RS    
        complex<double> asin_z = (-i) * std::log(i*z + std::sqrt(1.0-z*z));     
        return complex2scm(M_PI_2 - asin_z);
    } else {
        return double2scm(::acos(scm2double(n)));
    }
}

SchemeObject* s_tan(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"tan", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::tan(scm2complex(n)));
    } else {
        return double2scm(::tan(scm2double(n)));
    }
}

SchemeObject* s_atan(Scheme* scheme, SchemeObject* y, SchemeObject* x) {
    assert_arg_number_type(L"atan", 1, y);
    if (x == S_UNSPECIFIED) {
        if (y->type() == SchemeObject::COMPLEX_NUMBER) {
            complex<double> i = complex<double>(0,1);    
            complex<double> z = scm2complex(y);
            // Using the formula from R^5RS    
            return complex2scm((std::log(1.0+i*z) - std::log(1.0-i*z)) / (2.0*i));
        } else {
            return double2scm(::atan(scm2double(y)));
        }
    } else {
        assert_arg_real_type(L"atan", 1, y);
        assert_arg_real_type(L"atan", 2, x);
        return double2scm(::atan2(scm2double(y), scm2double(x)));
    }
}

// In R6RS log takes an optional base argument, which defaults to e for the natural logarithm.
// Note that log_b(x) = log_k(x) / log_k(b) for any base k
// For example log2(16) = log(16) / log(2) 
// TODO: Implement
SchemeObject* s_log(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"log", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::log(scm2complex(n)));
    } else if (n->type() == SchemeObject::INTEGER_NUMBER && scm2int(n) == 0) {
        throw scheme_exception(L"log",L"Not defined for zero.");
    } else {
        double re = scm2double(n);
        if (re >= 0) {
            return double2scm(::log(re));
        } else {
            return complex2scm(std::complex<double>(re, M_PI));
        }
    }
}

// Returns a^b
SchemeObject* s_expt(Scheme* scheme, SchemeObject* a, SchemeObject* b) {
    if (a->type() == SchemeObject::INTEGER_NUMBER && b->type() == SchemeObject::INTEGER_NUMBER) {
        int64_t bi = scm2int(b);
        int64_t ai = scm2int(a);    

        if (bi == 0) return int2scm(1);
        if (ai == 0) return int2scm(0);

        int64_t iter = ::llabs(bi);    
        int64_t result = iter % 2 ? ai : 1;
        
        while (iter >>= 1) {
            ai *= ai;        
            if (iter % 2) {
                result *= ai;    
            }
        }
        if (bi > 0) {
            return int2scm(result);
        } else {
            rational_type rational(1,result);
            return rational2scm(rational);        
        }
    } else if (a->type() == SchemeObject::RATIONAL_NUMBER && b->type() == SchemeObject::INTEGER_NUMBER) {
        return rational2scm(pow(scm2rational(a), scm2int(b)));
    } else {
        assert_arg_number_type(L"expt", 1, a);
        assert_arg_number_type(L"expt", 2, b);
        bool a_c = a->type() == SchemeObject::COMPLEX_NUMBER;
        bool b_c = b->type() == SchemeObject::COMPLEX_NUMBER;
        
        if (a_c) {
            if (b_c) {
                return complex2scm(std::pow(scm2complex(a), scm2complex(b)));        
            } else {
                if (b->type() == SchemeObject::INTEGER_NUMBER) {
                    return complex2scm(std::pow(scm2complex(a), scm2int(b)));
                } else {   
                    return complex2scm(std::pow(scm2complex(a), scm2double(b)));        
                }
            }        
        } else {
            if (b_c) {
                return complex2scm(std::pow(scm2double(a), scm2complex(b))); 
            } else {
                return double2scm(pow(scm2double(a),scm2double(b)));
            }           
        }
    }
}

// Returns e^n
SchemeObject* s_exp(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"exp", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::exp(scm2complex(n)));
    } else {
	double d = scm2double(n);
	if (std::isinf(d)) {
	    return double2scm(d > 0 ? -log(0.0) : 0.0);
	} else {
	    return double2scm(std::exp(d));
	}
    }
}

// Round returns the closest integer to x, rounding to even when x is halfway between two integers.
SchemeObject* s_round(Scheme* scheme, SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
     	return int2scm(round(scm2rational(n)));
    } else {
        assert_arg_real_type(L"round", 1, n);
        double nn = scm2double(n);
        double flo = floor(nn);
        double cei = ceil(nn);
        double dflo = nn - flo;
        double dcei = cei - nn;
        double result;
        if (dflo > dcei) {
            result = cei;
        } else if (dcei > dflo) {
            result = flo;
        } else {
            if(fmod(flo, 2) == 0) {
                 result = flo;
            } else {
                 result = cei;
            }
        }
        return double2scm(result);
    }
}

// Ceiling returns the smallest integer not smaller than x
SchemeObject* s_ceiling(Scheme* scheme, SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
	    return int2scm(ceil(scm2rational(n)));
    } else {
        assert_arg_real_type(L"ceiling", 1, n);
        return double2scm(ceil(scm2double(n)));
    }
}

// Floor returns the largest integer not larger than x
SchemeObject* s_floor(Scheme* scheme, SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
	    return int2scm(floor(scm2rational(n)));
    } else {
        assert_arg_real_type(L"floor", 1, n);
        return double2scm(floor(scm2double(n)));
    }
}

// Truncate returns the integer closest to x whose absolute value is not larger than the absolute value of x
SchemeObject* s_truncate(Scheme* scheme, SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
	    return int2scm(trunc(scm2rational(n)));
    } else {
        assert_arg_real_type(L"truncate", 1, n);
        double t = scm2double(n);
        #if HAVE_TRUNC
            double r = trunc(t);
        #else
            double r = t < 0 ? -floor(-t) : floor(t);
        #endif    
        return double2scm(r);
    }        
}

// http://groups.google.dk/group/sci.math/msg/f62e0bfcd24264f0?
//
// An algorithm which does give the correct answer reasonably quickly
// is as follows: First, find out whether there is an integer within y
// of x; if so, the answer will be an integer, and it is easy to find out
// which one.  Otherwise, let p1 and p2 be the integers on either side
// of x, and let q1 := q2 := 1, so that p1/q1 < x < p2/q2.  Now repeat
// the following as many times as necessary: Let p3 := p1+p2 and q3 := q1+q2.
// If p3/q3 is within y of x, then it is the correct answer.  Otherwise,
// if p3/q3 < x, let p1 := p3 and q1 := q3; if p3/q3 > x, let p2 := p3 and
// q2 := q3.  This gives a smaller interval (p1/q1,p2/q2) still containing x.

SchemeObject* s_rationalize(Scheme* scheme, SchemeObject* s_x, SchemeObject* s_y) {
    assert_arg_real_type(L"rationalize", 1, s_x);
    assert_arg_real_type(L"rationalize", 2, s_y);
    
    // TODO: inexact->exact returns large numerators and denominators
    // making the following overflow.
    bool exact = true;
    if (s_x->type() == SchemeObject::REAL_NUMBER) {
        exact = false;
        s_x = s_inexact_2_exact(scheme,s_x);
    }
    if (s_y->type() == SchemeObject::REAL_NUMBER) {
        exact = false;
        s_y = s_inexact_2_exact(scheme,s_y);
    } 
    
    rational_type x = scm2rational(s_x);
    rational_type::value_type sign = x >= rational_type::value_type(0) ? 1 : -1;
    x = abs(x);
    rational_type y = abs(scm2rational(s_y));
    rational_type p1 = floor(x);
    rational_type p2 = ceil(x);
    
    if (x - y < p1) {
        rational_type::value_type result = sign * ceil(x - y);
        return exact ? int2scm(result) : double2scm(double(result));
    }
    
    while (true) {
        rational_type p3(p1.numerator()+p2.numerator(),p1.denominator()+p2.denominator());
        if (abs(p3 - x) <= y) {
            rational_type result = sign * p3;
            return exact ? rational2scm(result) : double2scm(result.real()) ;
        }
        if (p3 < x) { 
            p1 = p3;
        } else {
            p2 = p3;
        }
    }
}


SchemeObject* s_quotient(Scheme* scheme, SchemeObject* n1, SchemeObject* n2) {
    assert_arg_int_type(L"quotient", 1, n1);
    assert_arg_int_type(L"quotient", 2, n2);
    int64_t nn1 = scm2int(n1);
    int64_t nn2 = scm2int(n2);
    int64_t result = nn1 / nn2;
    if (s_inexact_p(scheme,n1) == S_TRUE || s_inexact_p(scheme,n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}

SchemeObject* s_remainder(Scheme* scheme, SchemeObject* n1, SchemeObject* n2) {
    assert_arg_int_type(L"remainder", 1, n1);
    assert_arg_int_type(L"remainder", 2, n2);
    int64_t nn1 = scm2int(n1);
    int64_t nn2 = scm2int(n2);
    int64_t result = nn1 % nn2;
    if (result > 0) {
        if (nn1 < 0) {
            result -= labs(nn2);
        }
    } else if (result < 0) {
        if (nn1 > 0) {
            result += labs(nn2);
        }
    }
    if (s_inexact_p(scheme,n1) == S_TRUE || s_inexact_p(scheme,n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}

// The R^6RS procedure.
// TODO: Write a test for this. 
SchemeObject* s_mod(Scheme* scheme, SchemeObject* n1, SchemeObject* n2) {
    assert_arg_real_type(L"mod", 1, n1);
    assert_arg_real_type(L"mod", 2, n2);
    double nn1 = scm2double(n1);
    double nn2 = scm2double(n2);
    return double2scm(::fmod(nn1,nn2));
}

SchemeObject* s_modulo(Scheme* scheme, SchemeObject* n1, SchemeObject* n2) {
    assert_arg_int_type(L"modulo", 1, n1);
    assert_arg_int_type(L"modulo", 2, n2);
    int64_t nn1 = scm2int(n1);
    int64_t nn2 = scm2int(n2);
    int64_t result = nn1 % nn2;
    if (result * nn2 < 0) {
        if (result > 0) {
            result -= labs(nn2);
        } else {
            result += labs(nn2);
        }
    }
    if (s_inexact_p(scheme, n1) == S_TRUE || s_inexact_p(scheme, n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}


SchemeObject* s_min(Scheme* scheme, int num, SchemeStack::iterator stack) {
    assert (num > 0);
    SchemeObject::ObjectType outType = representativeNumberType(L"min", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
        double* args = (double*)::alloca(sizeof(double)*num);
        coerceNumbers(num,stack,args);
        double result = args[0];
        for(int i = 1; i < num; i++) {
            if (args[i] < result) {
                result = args[i];         
            }    
        }
        return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
       coerceNumbers(num,stack,args);
       int64_t result = args[0];
       int index = 0;
       for(int i = 1; i < num; i++) {
           if (args[i] < result) {
               result = args[i];
               index = i;
           }    
       }
       return stack[index];
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
       coerceNumbers(num,stack,args);
       rational_type result = args[0];
       int index = 0;
       for(int i = 1; i < num; i++) {
           if (args[i] < result) {
               result = args[i];
               index = i;
           }    
       }
       return stack[index];
    } else {
        throw scheme_exception(L"min", L"Wrong argument type");    
    }
}

SchemeObject* s_max(Scheme* scheme, int num, SchemeStack::iterator stack) {
    assert (num > 0);
    SchemeObject::ObjectType outType = representativeNumberType(L"max", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
        double* args = (double*)::alloca(sizeof(double)*num);
        coerceNumbers(num,stack,args);
        double result = args[0];
        for(int i = 1; i < num; i++) {
            if (args[i] > result) {
                result = args[i];         
            }    
        }
        return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
       coerceNumbers(num,stack,args);
       int64_t result = args[0];
       int index = 0;
       for(int i = 1; i < num; i++) {
           if (args[i] > result) {
               result = args[i];
               index = i;
           }    
       }
       return stack[index];
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
       coerceNumbers(num,stack,args);
       rational_type result = args[0];
       int index = 0;
       for(int i = 1; i < num; i++) {
           if (args[i] > result) {
               result = args[i];
               index = i;
           }    
       }
       return stack[index];
    } else {
       throw scheme_exception(L"max", L"Wrong argument type");    
    }
}

// Euclids algorithm 
int64_t i_gcd(int64_t a, int64_t b) {
    int64_t t = a;
    while(b != 0) {
        t = b;
        b = a % b;
        a = t;
    }
    return t;
}

// Using that gcd is associative thus gcd(a,b,c) = gcd(a,(gcd(b,c))) = gcd(gcd(a,b),c).
SchemeObject* s_gcd(Scheme* scheme, int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return int2scm(0);
    }
    SchemeObject::ObjectType outType = representativeNumberType(L"gcd", num, stack);

    assert_arg_int_type(L"gcd", 1, *stack);
    int64_t result = labs(scm2int(*stack));
    stack++;    
    for(int i = 1; i < num; i++) {
        assert_arg_int_type(L"gcd", i+1, *stack);
        int64_t n = scm2int(*stack);
        result = labs(i_gcd(result,n));
        stack++;    
    }
    // TODO: Make a i_exact_p macro and a representativeExactness("procname",num,stack) method
    if (outType == SchemeObject::INTEGER_NUMBER || outType == SchemeObject::RATIONAL_NUMBER) {
        return int2scm(result);    
    } else {
        return double2scm(double(result));    
    }
}

// Using the property gcd(a,b) * lcm(a,b) = a * b and that lcm(a,b,c) = lcm(lcm(a,b),c) = lcm(a,lcm(b,c))
SchemeObject* s_lcm(Scheme* scheme, int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return int2scm(1);
    }
    SchemeObject::ObjectType outType = representativeNumberType(L"lcm", num, stack);

    assert_arg_int_type(L"lcm", 1, *stack);
    int64_t result = labs(scm2int(*stack));
    stack++;
    for(int i = 1; i < num; i++) {
        assert_arg_int_type(L"lcm", i+1, *stack);
        int64_t n = labs(scm2int(*stack));
        int64_t g = i_gcd(n, result);
        result = g == 0 ? 0 : (n / g) * result;
        stack++;
    }
    // TODO: Make a i_exact_p macro and a representativeExactness("procname",num,stack) method
    if (outType == SchemeObject::INTEGER_NUMBER || outType == SchemeObject::RATIONAL_NUMBER) {
        return int2scm(result);    
    } else {
        return double2scm(double(result));    
    }
}

SchemeObject* s_numerator(Scheme* scheme, SchemeObject* n) {
    assert_arg_real_type(L"numerator", 1, n);
    if (s_exact_p(scheme,n) == S_TRUE) {
	    return int2scm(scm2rational(n).normalized().numerator());
    } else {
        SchemeObject* z = s_inexact_2_exact(scheme,n);
	    rational_type::value_type l = z->rationalValue().normalized().numerator();
        return double2scm(double(l));
    }
}

SchemeObject* s_denominator(Scheme* scheme, SchemeObject* n) {
    assert_arg_real_type(L"denominator", 1, n);
    if (s_exact_p(scheme,n) == S_TRUE) {
	    return int2scm(scm2rational(n).normalized().denominator());
    } else {
        SchemeObject* z = s_inexact_2_exact(scheme,n);
	    rational_type::value_type l = z->rationalValue().normalized().denominator();
        return double2scm(double(l));
    }
}

SchemeObject* s_make_polar(Scheme*, SchemeObject* magnitude, SchemeObject* angle) {
    assert_arg_real_type(L"make-polar", 1, magnitude);
    assert_arg_real_type(L"make-polar", 2, angle);
    std::complex<double> z = std::polar(magnitude->realValue(), angle->realValue());
    return complex2scm(z);
}

SchemeObject* s_make_rectangular(Scheme*, SchemeObject* real, SchemeObject* imag) {
    assert_arg_real_type(L"make-rectangular", 1, real);
    assert_arg_real_type(L"make-rectangular", 2, imag);
    std::complex<double> z(real->realValue(), imag->realValue());
    return complex2scm(z);
}

SchemeObject* s_real_part(Scheme*, SchemeObject* z) {
    assert_arg_complex_type(L"real-part", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(zz.real());
}

SchemeObject* s_imag_part(Scheme*, SchemeObject* z) {
    assert_arg_complex_type(L"imag-part", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(zz.imag());
}

SchemeObject* s_magnitude(Scheme*, SchemeObject* z) {
    assert_arg_complex_type(L"magnitude", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(std::abs(zz));        
}

SchemeObject* s_angle(Scheme* scheme, SchemeObject* z) {
    assert_arg_complex_type(L"angle", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(::atan2(zz.imag(), zz.real()));
}

SchemeObject* s_even_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_int_type(L"even?", 1, n);
    return (scm2int(n) & 0x1) == 0 ? S_TRUE : S_FALSE;
}

SchemeObject* s_odd_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_int_type(L"odd?", 1, n);
    return (scm2int(n) & 0x1) == 1 ? S_TRUE : S_FALSE;
}

SchemeObject* s_zero_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"zero?", 1, n);
    bool result;
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        result = scm2int(n) == 0;    
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        result = scm2rational(n) == rational_type::value_type(0);
    } else if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        std::complex<double> c = n->complexValue();
        result = c.real() == 0.0 && c.imag() == 0.0;
    } else {
        double d = scm2double(n);
        result = !std::isnan(d) && d == 0.0;
    }
    return bool2scm(result);
}

SchemeObject* s_infinite_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"infinite?", 1, n);
    if (n->type() == SchemeObject::REAL_NUMBER) {
	    return bool2scm(std::isinf(scm2double(n)));
    } else {
	    return S_FALSE;
    }
}

SchemeObject* s_finite_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"finite?", 1, n);
    if (n->type() == SchemeObject::REAL_NUMBER) {
	    return bool2scm(!std::isinf(scm2double(n)));
    } else {
	    return S_TRUE;
    }
}

SchemeObject* s_nan_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"finite?", 1, n);
    if (n->type() == SchemeObject::REAL_NUMBER) {
	    return bool2scm(std::isnan(scm2double(n)));
    } else {
	    return S_FALSE;
    }
}


SchemeObject* s_negative_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_real_type(L"negative?", 1, n);
    bool result;
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        result = scm2int(n) < 0;    
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        result = scm2rational(n) < rational_type::value_type(0);
    } else {
        result = scm2double(n) < 0.0;
    }
    return bool2scm(result);
}

SchemeObject* s_positive_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_real_type(L"positive?", 1, n);
    bool result;
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        result = scm2int(n) > 0;    
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        result = scm2rational(n) > rational_type::value_type(0);
    } else {
        result = scm2double(n) > 0.0;
    }
    return bool2scm(result);
}

// (= a b)
SchemeObject* s_equal(Scheme* scheme, int num, SchemeStack::iterator stack) {
    if (num == 0)  return S_TRUE;
    SchemeObject::ObjectType outType = representativeNumberType(L"=", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
        double* args = (double*)::alloca(sizeof(double)*num);
        coerceNumbers(num,stack,args);
        if (std::isnan(args[0])) return S_FALSE;
        for(int i = 1; i < num; i++) {
 	        if (std::isnan(args[i]) || args[i] != args[i-1]) return S_FALSE;
        }
	    return S_TRUE;
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
       coerceNumbers(num,stack,args);
       for(int i = 1; i < num; i++) {
	       if (args[i] != args[i-1]) return S_FALSE;
       }
       return S_TRUE;
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
       coerceNumbers(num,stack,args);
       for(int i = 1; i < num; i++) {
	       if (args[i] != args[i-1]) return S_FALSE;
       }
       return S_TRUE;
    } else {
       std::complex<double>* args = (std::complex<double>*)::alloca(sizeof(std::complex<double>)*num);
       coerceNumbers(num,stack,args);
       for(int i = 1; i < num; i++) {
	       if (args[i] != args[i-1]) return S_FALSE;
       }
       return S_TRUE;
    }
}

SchemeObject* s_less(Scheme* scheme, int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return S_TRUE;
    }

    SchemeObject::ObjectType outType = representativeNumberType(L"<", num, stack);
    
    if (outType == SchemeObject::REAL_NUMBER) {
        double* args = (double*)::alloca(sizeof(double)*num);
        coerceNumbers(num,stack,args);
        if (std::isnan(args[0])) return S_FALSE;
        for(int i = 1; i < num; i++) {
 	        if (std::isnan(args[i]) || args[i-1] >= args[i]) return S_FALSE;
        }
	    return S_TRUE;
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
        int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
        coerceNumbers(num,stack,args);
        for(int i = 1; i < num; i++) {
 	        if (args[i-1] >= args[i]) return S_FALSE;
        }
        return S_TRUE;
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
        rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
        coerceNumbers(num,stack,args);
        for(int i = 1; i < num; i++) {
 	        if (args[i-1] >= args[i]) return S_FALSE;
        }
        return S_TRUE;
    } else {
        throw scheme_exception(L"<", L"Wrong argument type");    
    }
}

SchemeObject* s_less_equal(Scheme* scheme, int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return S_TRUE;
    }
    SchemeObject::ObjectType outType = representativeNumberType(L"<=", num, stack);
    
    if (outType == SchemeObject::REAL_NUMBER) {
        double* args = (double*)::alloca(sizeof(double)*num);
        coerceNumbers(num,stack,args);
        if (std::isnan(args[0])) return S_FALSE;
        for(int i = 1; i < num; i++) {
 	        if (std::isnan(args[i]) || args[i-1] > args[i]) return S_FALSE;
        }
	    return S_TRUE;
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
        int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
        coerceNumbers(num,stack,args);
        for(int i = 1; i < num; i++) {
 	        if (args[i-1] > args[i]) return S_FALSE;
        }
        return S_TRUE;
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
        rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
        coerceNumbers(num,stack,args);
        for(int i = 1; i < num; i++) {
 	        if (args[i-1] > args[i]) return S_FALSE;
        }
        return S_TRUE;
    } else {
        throw scheme_exception(L"<=", L"Wrong argument type");    
    }
}

SchemeObject* s_greater(Scheme* scheme, int num, SchemeStack::iterator stack) {        
    if (num == 0) {
        return S_TRUE;
    }
    SchemeObject::ObjectType outType = representativeNumberType(L">", num, stack);
    
    if (outType == SchemeObject::REAL_NUMBER) {
        double* args = (double*)::alloca(sizeof(double)*num);
        coerceNumbers(num,stack,args);
        if (std::isnan(args[0])) return S_FALSE;
        for(int i = 1; i < num; i++) {
 	        if (std::isnan(args[i]) || args[i-1] <= args[i]) return S_FALSE;
        }
	    return S_TRUE;
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
        int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
        coerceNumbers(num,stack,args);
        for(int i = 1; i < num; i++) {
 	        if (args[i-1] <= args[i]) return S_FALSE;
        }
        return S_TRUE;
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
        rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
        coerceNumbers(num,stack,args);
        for(int i = 1; i < num; i++) {
 	        if (args[i-1] <= args[i]) return S_FALSE;
        }
        return S_TRUE;
    } else {
        throw scheme_exception(L">", L"Wrong argument type");    
    }
}

SchemeObject* s_greater_equal(Scheme* scheme, int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return S_TRUE;
    }

    SchemeObject::ObjectType outType = representativeNumberType(L"<=", num, stack);
    
    if (outType == SchemeObject::REAL_NUMBER) {
        double* args = (double*)::alloca(sizeof(double)*num);
        coerceNumbers(num,stack,args);
        if (std::isnan(args[0])) return S_FALSE;
        for(int i = 1; i < num; i++) {
 	        if (std::isnan(args[i]) || args[i-1] < args[i]) return S_FALSE;
        }
	    return S_TRUE;
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
        int64_t* args = (int64_t*)::alloca(sizeof(int64_t)*num);
        coerceNumbers(num,stack,args);
        for(int i = 1; i < num; i++) {
 	        if (args[i-1] < args[i]) return S_FALSE;
        }
        return S_TRUE;
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
        rational_type* args = (rational_type*)::alloca(sizeof(rational_type)*num);
        coerceNumbers(num,stack,args);
        for(int i = 1; i < num; i++) {
 	        if (args[i-1] < args[i]) return S_FALSE;
        }
        return S_TRUE;
    } else {
        throw scheme_exception(L">=", L"Wrong argument type");    
    }
}

// (number? p)
SchemeObject* s_number_p(Scheme* scheme, SchemeObject* p) {
    return i_number_p(p);
}

SchemeObject* i_complex_p(SchemeObject* n) {
    return i_number_p(n);
}

SchemeObject* s_complex_p(Scheme* scheme, SchemeObject* n) {
    return i_number_p(n);
}

SchemeObject* i_real_p(SchemeObject* n) {
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return n->complexValue().imag() == 0.0 ? S_TRUE : S_FALSE;
    } else {
        return i_number_p(n);
    }
}

SchemeObject* s_real_p(Scheme* scheme, SchemeObject* n) {
    return i_real_p(n);
}

SchemeObject* s_rational_p(Scheme* scheme, SchemeObject* n) {
    if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        return S_TRUE;
    } else if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return S_TRUE;    
    } else {
        return S_FALSE;    
    }
}

SchemeObject* i_integer_p(SchemeObject* p) {
    double i;
    if (p->type() == SchemeObject::INTEGER_NUMBER) {
        return S_TRUE;
    } else if (p->type() == SchemeObject::RATIONAL_NUMBER) {
	    rational_type r = p->rationalValue().normalized();     
	    rational_type::value_type d = r.denominator();     
        return bool2scm(d == 1 || d == -1);
    } else if (p->type() == SchemeObject::REAL_NUMBER) {
        double d = scm2double(p);
        return !std::isnan(d) && !std::isinf(d) && ::modf(d,&i) == 0.0 ? S_TRUE : S_FALSE;
    } else if (p->type() == SchemeObject::COMPLEX_NUMBER) {
        std::complex<double> z = p->complexValue();
        return ::modf(z.real(),&i) == 0.0 && z.imag() == 0.0 ? S_TRUE : S_FALSE;
    } else {
        return S_FALSE;    
    }
}

// (integer? p)
SchemeObject* s_integer_p(Scheme* scheme, SchemeObject* p) {
    return i_integer_p(p);
}

SchemeObject* s_exact_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"exact?", 1, n);
    return n->type() == SchemeObject::INTEGER_NUMBER ||
           n->type() == SchemeObject::RATIONAL_NUMBER ? S_TRUE : S_FALSE;
}

SchemeObject* s_inexact_p(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"inexact?", 1, n);
    return s_exact_p(scheme,n) == S_TRUE ? S_FALSE : S_TRUE;
}

SchemeObject* s_exact_2_inexact(Scheme* scheme, SchemeObject* n) {
    assert_arg_number_type(L"exact->inexact", 1, n);
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return double2scm(n->realValue());
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        return double2scm(n->realValue());
    } else  {
        return n;
    }
}

SchemeObject* i_double_2_exact(const double& n) {
    // Asserting that doubles are encoded as per the IEEEE 754 
    // standard, also known as IEC 559.
    assert(numeric_limits<double>::is_iec559);
    assert(sizeof(double) == sizeof(uint64_t));
    union {
	uint64_t d;
	double d_as_double;
    };
    d_as_double = n;
    int32_t sign = ((d >> 63) & 1) ? -1 : 1;
    int64_t exponent = ((d >> 52) & 0x07ff);
    uint64_t bit53 = 1;
    bit53 <<= 52;
    uint64_t mantissa = d & (bit53 - 1);
    if (exponent != 0 && exponent < 2047) {
        mantissa |= bit53;
        exponent -= 1023;
    } else {
        exponent = -1024;        
    }
    /*
    cout << "Sign: " << hex << sign << endl;
    cout << "Exponent: " << exponent << endl;
    cout << "Mantissa: " << mantissa << endl;
    */
    if (abs(exponent) > 63) {
         // TODO: Create rational<bigint>   
    }
    rational_type result = rational_type(sign * mantissa, bit53);
    result *= pow(rational_type(2), exponent);
    return rational2scm(result);
}

SchemeObject* s_inexact_2_exact(Scheme* scheme, SchemeObject* n) {
    assert_arg_real_type(L"inexact->exact", 1, n);
    if (n->type() == SchemeObject::REAL_NUMBER || n->type() == SchemeObject::COMPLEX_NUMBER) {
        return i_double_2_exact(scm2double(n));
    } else {
        return n;    
    }
}


///////////////////////////////////////////////////
// string->number
///////////////////////////////////////////////////

int extractDigit(wchar_t c, uint32_t radix) {
    int result = -1;
    result = c - L'0';
    if (radix > 10) {
	    if (result < 0 || result > 9) {
	        result = 10 + (towlower(c) - L'a');
	    }
    }
    
    // Check result
    if (result >= 0 && result < int(radix)) {
	    return result;
    } else {
	    return -1;
    }
}

char digitToChar(int d, uint32_t radix) {
    if (radix > 10 && d > 9) {
	    return 'a' + (d - 10);
    } else {
	    return '0' + d;
    }
}

// See http://jeffreystedfast.blogspot.com/2007/11/parsing-integers.html
// for a rock solid integer parser in C.
string extractDigits(wstring s, size_t offset, uint32_t radix) {
    string result;
    uint32_t i = 0;
    while (offset + i < s.size()) {
	    wchar_t digit = s[offset+i];
	    int d = extractDigit(digit, radix);
	    if (d == -1) {
	        return result;
	    }
	    result += digitToChar(d, radix);
	    i++;
    }
    return result;
}

/// Returns a number object or S_FALSE in case of failure
SchemeObject* i_string_2_number(Scheme* scheme, wstring s, uint32_t radix, size_t offset) {
    int sign = 1;
    if (s.size() == 0) {
	    return S_FALSE;
    }
    if (s == L"+inf.0") {
	    return double2scm(INFINITY);
    } else if (s == L"-inf.0") {
	    return double2scm(-INFINITY);
    } else if (s == L"+nan.0") {
        return double2scm(NAN);
    }

    bool radix_prefix_seen = false;
    bool exactness_prefix_seen = false;
    bool force_inexact = false;
    bool force_exact = false;
    while (s[offset] == L'#' && s.size() > offset+2) {
	    wchar_t c = ::towlower(s[offset+1]);
	    if (!radix_prefix_seen && (c == 'x' || c == 'd' || c == 'b' || c == 'o')) {
	        switch(c) {
	            case L'X' :        
                case L'x' : radix = 16;
                            radix_prefix_seen = true;
	        		        break;
	            case L'D' :        
                case L'd' : radix = 10;
                            radix_prefix_seen = true;
	        		        break;
	            case L'B' :        
                case L'b' : radix = 2;
                            radix_prefix_seen = true;
	        		        break;
	            case L'O' :        
                case L'o' : radix = 8;
                            radix_prefix_seen = true;
	        		        break;
	        }
	        offset += 2;
        } else if (!exactness_prefix_seen && (c == 'e' || c == 'i')) {
	        switch(c) {
                case L'E' :        
                case L'e' : force_exact = true;
                            exactness_prefix_seen = true;
                            break;
     	        case L'I' :        
                case L'i' : force_inexact = true;
                            exactness_prefix_seen = true;
                            break;
            }
	        offset += 2;
        } else {
	        return S_FALSE;
	    }
    } 
    
    bool sign_found = false;
    if (s[offset] == L'-') {
        offset++;   
        sign = -1;
        sign_found = true;
    } else if (s[offset] == L'+') {
        offset++;   
        sign_found = true;
    }
    if (offset >= s.size()) {
        return S_FALSE;
    }
    
    if (s[offset] == L'i' && sign_found == true) {
        offset++;
        if (offset >= s.size()) {
            return complex2scm(std::complex<double>(0.0,sign));        
        } else {
            return S_FALSE;        
        }
    }
    
    string digits = extractDigits(s, offset, radix);
    int64_t t = 0;
    if (digits.size() == 0) {
        t = 0;   
    } else {
        errno = 0;    
        t = strtoll(digits.c_str(), NULL, radix);
        if (errno == ERANGE) {
            // TODO: Create a bigint
            return S_FALSE;        
            //throw scheme_exception(L"Number out of range");
        }
    }

    offset += digits.size();
    if (offset >= s.size()) {
	    // Done. No . or e. We have an integer
        if (t > numeric_limits<int64_t>::max() ||
            t < numeric_limits<int64_t>::min()) {
            // TODO: Create a bigint            
            return S_FALSE;            
        } else {
	        if (force_inexact) {
                    return double2scm(double(sign*t));
	        } else {
                    return int2scm(sign*t);
	        }
        }
    }
    
    if (s[offset] == L'/') {
        offset++;
        string denominator = extractDigits(s, offset, radix);
        if (denominator.size() == 0) {
            return S_FALSE;        
        }
        errno = 0;
        int64_t d = strtol(denominator.c_str(), NULL, radix);
        if (errno == ERANGE) {
            // TODO: Create a bigint        
            return S_FALSE;        
            //throw scheme_exception(L"Number out of range");        
        }
        offset += denominator.size();
        if (offset >= s.size()) {
            if (d == 0) {
		        // TODO: return inexact +inf or -inf as per R^6RS
                return S_FALSE;        
            }        
	        rational_type result(sign*t, d);
	        if (force_inexact) {
               return double2scm(result.real());
	        } else {
               return rational2scm(result.normalized());
	        }
        } else {
            return S_FALSE;        
        }
    }
    
    double df = 0;
    string fraction;
    if (s[offset] == L'.' && radix == 10) {
        offset++;
        fraction = extractDigits(s, offset, 10);
        if (digits.size() == 0 && fraction.size() == 0) {
            // "." is not a number        
            return S_FALSE;        
        }
        
        int longlong_digits10 = numeric_limits<int64_t>::digits10;
        string clipped_fraction = fraction.substr(0,std::min(longlong_digits10,int(fraction.size())));
        errno = 0;    
        int64_t f = strtoll(clipped_fraction.c_str(), NULL, 10);
        if (errno == ERANGE) {
            return S_FALSE;
                   
        }
        df = double(f) / pow(10.0, double(clipped_fraction.size()));
        offset += fraction.size();
    }
    if (offset >= s.size()) {
	    // Done. No e
        double result = sign * (t + df);
        return force_exact ? i_double_2_exact(result) : double2scm(result);
    }
    
    int64_t e = 0;
    if ((digits.size() != 0 || fraction.size() != 0) && radix == 10 && 
            (s[offset] == L'e' || s[offset] == L's' || s[offset] == L'f' || 
             s[offset] == L'd' || s[offset] == L'l')) {
        offset++;
        int64_t esign = 1;
        if (s[offset] == '-') {
            esign = -1;
            offset++;
        }
        if (s[offset] == '+') {
            esign = 1;
            offset++;
        }
        string exponent = extractDigits(s, offset, 10);
        if (exponent.size() == 0) {
             return S_FALSE;
        }
        errno = 0;
        e = esign * strtol(exponent.c_str(), NULL, 10);
        if (errno == ERANGE) {
            return S_FALSE;        
        }
        if  (e < numeric_limits<double>::min_exponent10 ||
             e > numeric_limits<double>::max_exponent10) {
             return S_FALSE;     
        }

        if  (e < numeric_limits<double>::min_exponent10 ||
             e > numeric_limits<double>::max_exponent10) {
             return S_FALSE;     
        }
        
        offset += exponent.size();
    }
    if (offset >= s.size()) {
        double result = sign*(t + df) * pow(10,double(e));
        return force_exact ? i_double_2_exact(result) : double2scm(result);
    }
    
    if (s[offset] == L'i' && (digits.size() != 0 || fraction.size() != 0) && sign_found) {
        offset++;    
        if (offset >= s.size()) {
            std::complex<double> z(0, sign*(t + df) * pow(10,double(e)));    
            return complex2scm(z);
        } else {
            return S_FALSE;        
        }
    }
    
    if (s[offset] == L'@') {
        offset++;
        SchemeObject* s_angle = i_string_2_number(scheme, s, radix, offset);
        if (s_angle->type() != SchemeObject::COMPLEX_NUMBER) {
            double angle = s_angle->realValue();
            double magnitude = sign*(t + df) * pow(10,double(e));
            std::complex<double> z = std::polar(magnitude, angle);
            return complex2scm(z);
        } else {
            return S_FALSE;
        }
    }
    
    if (digits.size() != 0 || fraction.size() != 0) {
        SchemeObject* imag = i_string_2_number(scheme, s, radix, offset);
        if (imag->type() == SchemeObject::COMPLEX_NUMBER) {
           SchemeObject* real = SchemeObject::createRealNumber(sign*(t + df) * pow(10,double(e)));;    
           return SchemeObject::createComplexNumber(real, imag->imag);
        }
    }
    
    return S_FALSE;
}

wstring i_number_2_string(SchemeObject* o, uint32_t radix) {
    std::wostringstream ss;
    SchemeObject::ObjectType type = o->type();
    if (type == SchemeObject::INTEGER_NUMBER) {
        ss << std::setbase(radix) << scm2int(o);
        return ss.str();
    } else if (type == SchemeObject::RATIONAL_NUMBER) {
	    rational_type z = scm2rational(o).normalized();
	    ss << std::setbase(radix);
	    ss << z.numerator();
	    if (z.denominator() != 1) {
            ss << L"/";
   	        ss << z.denominator();
	    }
        return ss.str();
    } else if (type == SchemeObject::REAL_NUMBER) {
        double d = scm2double(o);
	    if (std::isinf(d)) {
	        ss << (d > 0 ? L"+inf.0" : L"-inf.0");
	    } else if (std::isnan(d)) {
	        ss << L"+nan.0";
	    } else {
	        ss << std::setbase(radix);
	        ss << std::setprecision(numeric_limits<double>::digits10);
	        ss << d;
	        double i;
	        // Append ".0" if d contains no decimal point.
	        if (::modf(d,&i) == 0.0) {
		        ss << L".0";        
	        }
	    }
	    return ss.str();
    } else if (type == SchemeObject::COMPLEX_NUMBER) {
        double imag = o->imag->realValue();
        if (imag == 0.0) {
            return i_number_2_string(o->real, radix);        
        } else {
            ss << i_number_2_string(o->real, radix);
            if (o->imag->realValue() >= 0.0 && !std::isinf(o->imag->realValue())) {
                ss << L"+";        
            }
            ss << i_number_2_string(o->imag, radix);
            ss << L"i";
            return ss.str();
        }
    } else {
        throw scheme_exception(L"Not a number");
    }
}

void LibNumbers::bind(Scheme* scheme, SchemeObject* envt) {
    scheme->assign(L"number?"               ,1,0,0, (SchemeObject* (*)()) s_number_p, envt);
    scheme->assign(L"integer?"              ,1,0,0, (SchemeObject* (*)()) s_integer_p, envt);
    scheme->assign(L"complex?"              ,1,0,0, (SchemeObject* (*)()) s_complex_p, envt);
    scheme->assign(L"real?"                 ,1,0,0, (SchemeObject* (*)()) s_real_p, envt);
    scheme->assign(L"rational?"             ,1,0,0, (SchemeObject* (*)()) s_rational_p, envt);
    scheme->assign(L"exact?"                ,1,0,0, (SchemeObject* (*)()) s_exact_p, envt);
    scheme->assign(L"inexact?"              ,1,0,0, (SchemeObject* (*)()) s_inexact_p, envt);

    // Comparators
    scheme->assign(L"="                     ,0,0,1, (SchemeObject* (*)()) s_equal, envt);
    scheme->assign(L"<"                     ,0,0,1, (SchemeObject* (*)()) s_less, envt);
    scheme->assign(L">"                     ,0,0,1, (SchemeObject* (*)()) s_greater, envt);
    scheme->assign(L"<="                    ,0,0,1, (SchemeObject* (*)()) s_less_equal, envt);
    scheme->assign(L">="                    ,0,0,1, (SchemeObject* (*)()) s_greater_equal, envt);
    scheme->assign(L"+"                     ,0,0,1, (SchemeObject* (*)()) s_plus, envt);
    scheme->assign(L"-"                     ,1,0,1, (SchemeObject* (*)()) s_minus, envt);
    scheme->assign(L"*"                     ,0,0,1, (SchemeObject* (*)()) s_mult, envt);
    scheme->assign(L"/"                     ,1,0,1, (SchemeObject* (*)()) s_divide, envt);

    scheme->assign(L"abs"                   ,1,0,0, (SchemeObject* (*)()) s_abs, envt);
    scheme->assign(L"tan"                   ,1,0,0, (SchemeObject* (*)()) s_tan, envt);
    scheme->assign(L"atan"                  ,1,1,0, (SchemeObject* (*)()) s_atan, envt);
    scheme->assign(L"sin"                   ,1,0,0, (SchemeObject* (*)()) s_sin, envt);
    scheme->assign(L"asin"                  ,1,0,0, (SchemeObject* (*)()) s_asin, envt);
    scheme->assign(L"cos"                   ,1,0,0, (SchemeObject* (*)()) s_cos, envt);
    scheme->assign(L"acos"                  ,1,0,0, (SchemeObject* (*)()) s_acos, envt);
    scheme->assign(L"sqrt"                  ,1,0,0, (SchemeObject* (*)()) s_sqrt, envt);
    scheme->assign(L"log"                   ,1,0,0, (SchemeObject* (*)()) s_log, envt);
    scheme->assign(L"exp"                   ,1,0,0, (SchemeObject* (*)()) s_exp, envt);
    scheme->assign(L"expt"                  ,2,0,0, (SchemeObject* (*)()) s_expt, envt);
    scheme->assign(L"exact-integer-sqrt"    ,1,0,0, (SchemeObject* (*)()) s_exact_integer_sqrt, envt);

    // Roundings
    scheme->assign(L"round"                 ,1,0,0, (SchemeObject* (*)()) s_round, envt);
    scheme->assign(L"ceiling"               ,1,0,0, (SchemeObject* (*)()) s_ceiling, envt);
    scheme->assign(L"floor"                 ,1,0,0, (SchemeObject* (*)()) s_floor, envt);
    scheme->assign(L"truncate"              ,1,0,0, (SchemeObject* (*)()) s_truncate, envt);

    scheme->assign(L"rationalize"           ,2,0,0, (SchemeObject* (*)()) s_rationalize, envt);
    scheme->assign(L"quotient"              ,2,0,0, (SchemeObject* (*)()) s_quotient, envt);
    scheme->assign(L"remainder"             ,2,0,0, (SchemeObject* (*)()) s_remainder, envt);
    scheme->assign(L"modulo"                ,2,0,0, (SchemeObject* (*)()) s_modulo, envt);
    scheme->assign(L"mod"                ,2,0,0, (SchemeObject* (*)()) s_mod, envt);
    
    scheme->assign(L"min"                   ,1,0,1, (SchemeObject* (*)()) s_min, envt);
    scheme->assign(L"max"                   ,1,0,1, (SchemeObject* (*)()) s_max, envt);
    scheme->assign(L"gcd"                   ,0,0,1, (SchemeObject* (*)()) s_gcd, envt);
    scheme->assign(L"lcm"                   ,0,0,1, (SchemeObject* (*)()) s_lcm, envt);
    scheme->assign(L"numerator"             ,1,0,0, (SchemeObject* (*)()) s_numerator, envt);
    scheme->assign(L"denominator"           ,1,0,0, (SchemeObject* (*)()) s_denominator, envt);
    scheme->assign(L"exact->inexact"        ,1,0,0, (SchemeObject* (*)()) s_exact_2_inexact, envt);
    scheme->assign(L"inexact->exact"        ,1,0,0, (SchemeObject* (*)()) s_inexact_2_exact, envt);
    scheme->assign(L"inexact"               ,1,0,0, (SchemeObject* (*)()) s_exact_2_inexact, envt);
    scheme->assign(L"exact"                 ,1,0,0, (SchemeObject* (*)()) s_inexact_2_exact, envt);
    scheme->assign(L"even?"                 ,1,0,0, (SchemeObject* (*)()) s_even_p, envt);
    scheme->assign(L"odd?"                  ,1,0,0, (SchemeObject* (*)()) s_odd_p, envt);
    scheme->assign(L"zero?"                 ,1,0,0, (SchemeObject* (*)()) s_zero_p, envt);
    scheme->assign(L"infinite?"             ,1,0,0, (SchemeObject* (*)()) s_infinite_p, envt);
    scheme->assign(L"finite?"               ,1,0,0, (SchemeObject* (*)()) s_finite_p, envt);
    scheme->assign(L"nan?"                  ,1,0,0, (SchemeObject* (*)()) s_nan_p, envt);
    scheme->assign(L"negative?"             ,1,0,0, (SchemeObject* (*)()) s_negative_p, envt);
    scheme->assign(L"positive?"             ,1,0,0, (SchemeObject* (*)()) s_positive_p, envt);

    // Complex numbers
    scheme->assign(L"make-rectangular"      ,2,0,0, (SchemeObject* (*)()) s_make_rectangular, envt);
    scheme->assign(L"make-polar"            ,2,0,0, (SchemeObject* (*)()) s_make_polar, envt);
    scheme->assign(L"real-part"             ,1,0,0, (SchemeObject* (*)()) s_real_part, envt);
    scheme->assign(L"imag-part"             ,1,0,0, (SchemeObject* (*)()) s_imag_part, envt);
    scheme->assign(L"magnitude"             ,1,0,0, (SchemeObject* (*)()) s_magnitude, envt);
    scheme->assign(L"angle"                 ,1,0,0, (SchemeObject* (*)()) s_angle, envt);
}
