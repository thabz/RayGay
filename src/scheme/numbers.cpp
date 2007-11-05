
#include "numbers.h"

#include <cmath>
#include <cerrno>
#include <iomanip>


void coerceNumbers(int num, SchemeStack::iterator stack, double* dest) {
    for(int i = 0; i < num; i++) {
        *dest = scm2double(*stack);
        stack++;
        dest++;
    }
}

void coerceNumbers(int num, SchemeStack::iterator stack, long* dest) {
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

// This returns the best number type for the numbers in stacks. Ie. if the stack contains all integers
// but just one rational, the rational is returned. If it's all rationals but with just one real, the
// real-type is returned. 
SchemeObject::ObjectType representativeNumberType(wchar_t* procname, int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType type = SchemeObject::INTEGER_NUMBER;
    for(int i = 0; i < num; i++) {
        SchemeObject* n = *stack;
        assert_arg_number_type(procname, i+1, n);
        if (n->type() < type) {
            type = n->type();        
        }
        stack++;    
    }
    return type;
}

SchemeObject* s_plus(int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"+", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
       double args[num];
       coerceNumbers(num,stack,args);
       
       double result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       
       long result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double> args[num];
       coerceNumbers(num,stack,args);
       
       std::complex<double> result = 0;
       for(int i = 0; i < num; i++) {
           result += args[i];    
       }
       return complex2scm(result);
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       rational_type args[num];
       coerceNumbers(num,stack,args);
       
       rational_type result(0);
       for(int i = 0; i < num; i++) {
	   result += args[i];
       }
       return rational2scm(result);
    } else {
        throw scheme_exception(L"+", L"Only integer and real support");    
    }
}

SchemeObject* s_minus(int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"-", num, stack);
    if (outType == SchemeObject::REAL_NUMBER) {
       double args[num];
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
       long args[num];
       coerceNumbers(num,stack,args);
       long result = args[0];
       if (num == 1) {
           // One-argument case is a simple negate (n => -n)
           return int2scm(-result);
       } 
       for(int i = 1; i < num; i++) {
           result -= args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double> args[num];
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
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       rational_type args[num];
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
    } else {
        throw scheme_exception(L"-", L"Only integer and real support");    
    }        
}

SchemeObject* s_divide(int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"/", num, stack);
    if (outType == SchemeObject::REAL_NUMBER) {
       double args[num];
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
       std::complex<double> args[num];
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
       rational_type args[num];
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

SchemeObject* s_mult(int num, SchemeStack::iterator stack) {
    SchemeObject::ObjectType outType = representativeNumberType(L"*", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
       double args[num];
       coerceNumbers(num,stack,args);
       
       double result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return double2scm(result);
            
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       
       long result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return int2scm(result);
    } else if (outType == SchemeObject::COMPLEX_NUMBER) {
       std::complex<double> args[num];
       coerceNumbers(num,stack,args);
       
       std::complex<double> result = 1;
       for(int i = 0; i < num; i++) {
           result *= args[i];    
       }
       return complex2scm(result);
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       rational_type args[num];
       coerceNumbers(num,stack,args);
       
       rational_type result(1);
       for(int i = 0; i < num; i++) {
	   result *= args[i]; 
       }
       return rational2scm(result);
    } else {
        throw scheme_exception(L"*", L"Only integer and real support");    
    }
}


SchemeObject* s_sqrt(SchemeObject* n) {
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

SchemeObject* s_abs(SchemeObject* n) {
    assert_arg_type(L"abs", 1, s_real_p, n);
    if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        return rational2scm(abs(scm2rational(n)));
    } else if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return int2scm(labs(scm2int(n)));
    } else {
        return double2scm(fabs(scm2double(n)));
    }         
}


SchemeObject* s_sin(SchemeObject* n) {
    assert_arg_number_type(L"sin", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::sin(scm2complex(n)));
    } else {
        return double2scm(::sin(scm2double(n)));
    }
}

SchemeObject* s_asin(SchemeObject* n) {
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

SchemeObject* s_cos(SchemeObject* n) {
    assert_arg_number_type(L"cos", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::cos(scm2complex(n)));
    } else {
        return double2scm(::cos(scm2double(n)));
    }
}

SchemeObject* s_acos(SchemeObject* n) {
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

SchemeObject* s_tan(SchemeObject* n) {
    assert_arg_number_type(L"tan", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::tan(scm2complex(n)));
    } else {
        return double2scm(::tan(scm2double(n)));
    }
}

SchemeObject* s_atan(SchemeObject* y, SchemeObject* x) {
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
        assert_arg_type(L"atan", 1, s_real_p, y);
        assert_arg_type(L"atan", 2, s_real_p, x);
        return double2scm(::atan2(scm2double(y), scm2double(x)));
    }
}

// In R6RS log takes an optional base argument, which defaults to e for the natural logarithm.
// Note that log_b(x) = log_k(x) / log_k(b) for any base k
// For example log2(16) = log(16) / log(2) 
// TODO: Implement
SchemeObject* s_log(SchemeObject* n) {
    assert_arg_number_type(L"log", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::log(scm2complex(n)));
    } else {
        return double2scm(::log(scm2double(n)));
    }
}

// Returns a^b
SchemeObject* s_expt(SchemeObject* a, SchemeObject* b) {
    if (a->type() == SchemeObject::INTEGER_NUMBER && b->type() == SchemeObject::INTEGER_NUMBER) {
        long ai = scm2int(a);    
        long bi = scm2int(b);
        long iter = labs(bi);    

        if (bi == 0) return S_ONE;

        // The following algorithm while slow, is OK for the 
	// relative small long values that b can hold without
	// we overflowing anyway.
        long result = 1;
        while (iter-- > 0) {
            result *= ai;        
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
SchemeObject* s_exp(SchemeObject* n) {
    assert_arg_number_type(L"exp", 1, n);
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return complex2scm(std::exp(scm2complex(n)));
    } else {
        return double2scm(::exp(scm2double(n)));
    }
}

// Round returns the closest integer to x, rounding to even when x is halfway between two integers.
SchemeObject* s_round(SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
     	return int2scm(round(scm2rational(n)));
    } else {
        assert_arg_type(L"round", 1, s_real_p, n);
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
SchemeObject* s_ceiling(SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
	return int2scm(ceil(scm2rational(n)));
    } else {
        assert_arg_type(L"ceiling", 1, s_real_p, n);
        return double2scm(ceil(scm2double(n)));
    }
}

// Floor returns the largest integer not larger than x
SchemeObject* s_floor(SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
	return int2scm(floor(scm2rational(n)));
    } else {
        assert_arg_type(L"floor", 1, s_real_p, n);
        return double2scm(floor(scm2double(n)));
    }
}

// Truncate returns the integer closest to x whose absolute value is not larger than the absolute value of x
SchemeObject* s_truncate(SchemeObject* n) {
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return n;
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
	return int2scm(trunc(scm2rational(n)));
    } else {
        assert_arg_type(L"truncate", 1, s_real_p, n);
        double t = scm2double(n);
        #if HAVE_TRUNC
            double r = trunc(t);
        #else
            double r = t < 0 ? -floor(-t) : floor(t);
        #endif    
        return double2scm(r);
    }        
}

SchemeObject* s_quotient(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type(L"quotient", 1, s_integer_p, n1);
    assert_arg_type(L"quotient", 2, s_integer_p, n2);
    long nn1 = scm2int(n1);
    long nn2 = scm2int(n2);
    long result = nn1 / nn2;
    if (s_inexact_p(n1) == S_TRUE || s_inexact_p(n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}

SchemeObject* s_remainder(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type(L"remainder", 1, s_integer_p, n1);
    assert_arg_type(L"remainder", 2, s_integer_p, n2);
    long nn1 = scm2int(n1);
    long nn2 = scm2int(n2);
    long result = nn1 % nn2;
    if (result > 0) {
        if (nn1 < 0) {
            result -= labs(nn2);
        }
    } else if (result < 0) {
        if (nn1 > 0) {
            result += labs(nn2);
        }
    }
    if (s_inexact_p(n1) == S_TRUE || s_inexact_p(n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}

SchemeObject* s_modulo(SchemeObject* n1, SchemeObject* n2) {
    assert_arg_type(L"modulo", 1, s_integer_p, n1);
    assert_arg_type(L"modulo", 2, s_integer_p, n2);
    long nn1 = scm2int(n1);
    long nn2 = scm2int(n2);
    long result = nn1 % nn2;
    if (result * nn2 < 0) {
        if (result > 0) {
            result -= labs(nn2);
        } else {
            result += labs(nn2);
        }
    }
    if (s_inexact_p(n1) == S_TRUE || s_inexact_p(n2) == S_TRUE) {
        return double2scm(double(result));
    } else {
        return int2scm(result);
    }
}


SchemeObject* s_min(int num, SchemeStack::iterator stack) {
    assert (num > 0);
    SchemeObject::ObjectType outType = representativeNumberType(L"min", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
        double args[num];
        coerceNumbers(num,stack,args);
        double result = args[0];
        for(int i = 1; i < num; i++) {
            if (args[i] < result) {
                result = args[i];         
            }    
        }
        return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       long result = args[0];
       int index = 0;
       for(int i = 1; i < num; i++) {
           if (args[i] < result) {
               result = args[i];
               index = i;
           }    
       }
       return stack[index];
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       rational_type args[num];
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

SchemeObject* s_max(int num, SchemeStack::iterator stack) {
    assert (num > 0);
    SchemeObject::ObjectType outType = representativeNumberType(L"max", num, stack);

    if (outType == SchemeObject::REAL_NUMBER) {
        double args[num];
        coerceNumbers(num,stack,args);
        double result = args[0];
        for(int i = 1; i < num; i++) {
            if (args[i] > result) {
                result = args[i];         
            }    
        }
        return double2scm(result);
    } else if (outType == SchemeObject::INTEGER_NUMBER) {
       long args[num];
       coerceNumbers(num,stack,args);
       long result = args[0];
       int index = 0;
       for(int i = 1; i < num; i++) {
           if (args[i] > result) {
               result = args[i];
               index = i;
           }    
       }
       return stack[index];
    } else if (outType == SchemeObject::RATIONAL_NUMBER) {
       rational_type args[num];
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
long i_gcd(long a, long b) {
    long t = a;
    while(b != 0) {
        t = b;
        b = a % b;
        a = t;
    }
    return t;
}

// Using that gcd is associative thus gcd(a,b,c) = gcd(a,(gcd(b,c))) = gcd(gcd(a,b),c).
SchemeObject* s_gcd(int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return S_ZERO;
    }
    SchemeObject::ObjectType outType = representativeNumberType(L"gcd", num, stack);

    assert_arg_int_type(L"gcd", 1, *stack);
    long result = labs(scm2int(*stack));
    stack++;    
    for(int i = 1; i < num; i++) {
        assert_arg_int_type(L"gcd", i+1, *stack);
        long n = scm2int(*stack);
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
SchemeObject* s_lcm(int num, SchemeStack::iterator stack) {
    if (num == 0) {
        return S_ONE;
    }
    SchemeObject::ObjectType outType = representativeNumberType(L"lcm", num, stack);

    assert_arg_int_type(L"lcm", 1, *stack);
    long result = labs(scm2int(*stack));
    stack++;
    for(int i = 1; i < num; i++) {
        assert_arg_int_type(L"lcm", i+1, *stack);
        long n = labs(scm2int(*stack));
        long g = i_gcd(n, result);
        result = g == 0 ? 0 : n * result / g;
        stack++;
    }
    // TODO: Make a i_exact_p macro and a representativeExactness("procname",num,stack) method
    if (outType == SchemeObject::INTEGER_NUMBER || outType == SchemeObject::RATIONAL_NUMBER) {
        return int2scm(result);    
    } else {
        return double2scm(double(result));    
    }
}

SchemeObject* s_numerator(SchemeObject* n) {
    assert_arg_type(L"numerator", 1, s_real_p, n);
    if (s_exact_p(n) == S_TRUE) {
	return int2scm(scm2rational(n).normalized().numerator());
    } else {
        SchemeObject* z = s_inexact_2_exact(n);
	rational_type::value_type l = z->rationalValue().normalized().numerator();
        return double2scm(double(l));
    }
}

SchemeObject* s_denominator(SchemeObject* n) {
    assert_arg_type(L"denominator", 1, s_real_p, n);
    if (s_exact_p(n) == S_TRUE) {
	return int2scm(scm2rational(n).normalized().denominator());
    } else {
        SchemeObject* z = s_inexact_2_exact(n);
	rational_type::value_type l = z->rationalValue().normalized().denominator();
        return double2scm(double(l));
    }
}

SchemeObject* s_make_polar(SchemeObject* magnitude, SchemeObject* angle) {
    assert_arg_type(L"make-polar", 1, s_real_p, magnitude);
    assert_arg_type(L"make-polar", 2, s_real_p, angle);
    std::complex<double> z = std::polar(magnitude->realValue(), angle->realValue());
    return complex2scm(z);
}

SchemeObject* s_make_rectangular(SchemeObject* real, SchemeObject* imag) {
    assert_arg_type(L"make-rectangular", 1, s_real_p, real);
    assert_arg_type(L"make-rectangular", 2, s_real_p, imag);
    std::complex<double> z(real->realValue(), imag->realValue());
    return complex2scm(z);
}

SchemeObject* s_real_part(SchemeObject* z) {
    assert_arg_complex_type(L"real-part", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(zz.real());
}

SchemeObject* s_imag_part(SchemeObject* z) {
    assert_arg_complex_type(L"imag-part", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(zz.imag());
}

SchemeObject* s_magnitude(SchemeObject* z) {
    assert_arg_complex_type(L"magnitude", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(std::abs(zz));        
}

SchemeObject* s_angle(SchemeObject* z) {
    assert_arg_complex_type(L"angle", 1, z);
    std::complex<double> zz = z->complexValue();
    return double2scm(::atan2(zz.imag(), zz.real()));
}

SchemeObject* s_even_p(SchemeObject* n) {
    assert_arg_int_type(L"even?", 1, n);
    return (scm2int(n) & 0x1) == 0 ? S_TRUE : S_FALSE;
}

SchemeObject* s_odd_p(SchemeObject* n) {
    assert_arg_int_type(L"odd?", 1, n);
    return (scm2int(n) & 0x1) == 1 ? S_TRUE : S_FALSE;
}

SchemeObject* s_zero_p(SchemeObject* n) {
    assert_arg_number_type(L"zero?", 1, n);
    bool result;
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        result = scm2int(n) == 0;    
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        result = scm2int(n->numerator) == 0;
    } else if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        std::complex<double> c = n->complexValue();
        result = c.real() == 0.0 && c.imag() == 0.0;
    } else {
        result = scm2double(n) == 0.0;
    }
    return bool2scm(result);
}

SchemeObject* s_negative_p(SchemeObject* n) {
    assert_arg_type(L"negative?", 1, s_real_p, n);
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

SchemeObject* s_positive_p(SchemeObject* n) {
    assert_arg_type(L"positive?", 1, s_real_p, n);
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
// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_equal(int num, SchemeStack::iterator args) {
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L"=", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L"=", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn != n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_less(int num, SchemeStack::iterator args) {
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L"<", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L"<", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn <= n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_less_equal(int num, SchemeStack::iterator args) {
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L"<=", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L"<=", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn < n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_greater(int num, SchemeStack::iterator args) {        
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L">", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L">", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn >= n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}

// FIXME: Numerical instable. Use integer and rational comparisons
SchemeObject* s_greater_equal(int num, SchemeStack::iterator args) {
    if (num == 0) {
        return S_TRUE;
    }
    assert_arg_type(L">=", 1, s_real_p, *args);
    double n = scm2double(*args);
    args ++;
    for(int i = 1; i < num; i++) {
        SchemeObject* v = *args++;
        assert_arg_type(L">=", i+1, s_real_p, v);
        double nn = scm2double(v);
        if (nn > n) {
            return S_FALSE;
        }
        n = nn;
    }
    return S_TRUE;
}


// (number? p)
SchemeObject* s_number_p(SchemeObject* p) {
    return i_number_p(p);
}


SchemeObject* s_complex_p(SchemeObject* n) {
    return i_number_p(n);
}

SchemeObject* s_real_p(SchemeObject* n) {
    if (n->type() == SchemeObject::COMPLEX_NUMBER) {
        return n->complexValue().imag() == 0.0 ? S_TRUE : S_FALSE;
    } else {
        return i_number_p(n);
    }
}

SchemeObject* s_rational_p(SchemeObject* n) {
    if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        return S_TRUE;
    } else if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return S_TRUE;    
    } else {
        return S_FALSE;    
    }
}

// (integer? p)
SchemeObject* s_integer_p(SchemeObject* p) {
    double i;
    if (p->type() == SchemeObject::INTEGER_NUMBER) {
        return S_TRUE;
    } else if (p->type() == SchemeObject::RATIONAL_NUMBER) {
	rational_type::value_type denom = p->rationalValue().denominator();     
        return bool2scm(denom == 1 || denom == -1);
    } else if (p->type() == SchemeObject::REAL_NUMBER) {
        return ::modf(scm2double(p),&i) == 0.0 ? S_TRUE : S_FALSE;
    } else if (p->type() == SchemeObject::COMPLEX_NUMBER) {
        std::complex<double> z = p->complexValue();
        return ::modf(z.real(),&i) == 0.0 && z.imag() == 0.0 ? S_TRUE : S_FALSE;
    } else {
        return S_FALSE;    
    }
}

SchemeObject* s_exact_p(SchemeObject* n) {
    assert_arg_type(L"exact?", 1, s_number_p, n);
    return n->type() == SchemeObject::INTEGER_NUMBER ||
           n->type() == SchemeObject::RATIONAL_NUMBER ? S_TRUE : S_FALSE;
}

SchemeObject* s_inexact_p(SchemeObject* n) {
    assert_arg_type(L"inexact?", 1, s_number_p, n);
    return s_exact_p(n) == S_TRUE ? S_FALSE : S_TRUE;
}

SchemeObject* s_exact_2_inexact(SchemeObject* n) {
    assert_arg_type(L"exact->inexact", 1, s_number_p, n);
    if (n->type() == SchemeObject::INTEGER_NUMBER) {
        return double2scm(n->realValue());
    } else if (n->type() == SchemeObject::RATIONAL_NUMBER) {
        return double2scm(n->realValue());
    } else  {
        return n;
    }
}

SchemeObject* s_inexact_2_exact(SchemeObject* n) {
    assert_arg_type(L"inexact->exact", 1, s_real_p, n);
    if (n->type() == SchemeObject::REAL_NUMBER || n->type() == SchemeObject::COMPLEX_NUMBER) {
        // To converter a double to a rational, we multiply
        // it with 10 until it's an integer number.
        double real = n->realValue();
        double tmp;
	rational_type::value_type denominator = 1;
        while (::modf(real,&tmp) != 0.0) {
            real *= 10;
            denominator *= 10;
        }
        rational_type::value_type numerator = rational_type::value_type(real);
        rational_type rational(numerator, denominator);
        return rational2scm(rational);
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
SchemeObject* i_string_2_number(wstring s, uint32_t radix, size_t offset) {
    int sign = 1;
    if (s.size() == 0) {
	return S_FALSE;
    }
    if (s[offset] == L'#' && s.size() > offset+2) {
	wchar_t prefix = s[offset+1];
	switch(prefix) {
            case L'x' : radix = 16;
			break;
            case L'd' : radix = 10;
			break;
            case L'b' : radix = 2;
			break;
            case L'o' : radix = 8;
			break;
	    case L'e' :
            case L'i' : break;
	}
	offset += 2;
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
            return SchemeObject::createComplexNumber(std::complex<double>(0.0,sign));        
        } else {
            return S_FALSE;        
        }
    }
    
    string digits = extractDigits(s, offset, radix);
    long t;
    if (digits.size() == 0) {
        t = 0;   
    } else {
        errno = 0;    
        t = strtol(digits.c_str(), NULL, radix);
        if (errno == ERANGE) {
            // TODO: Create a bigint
            return S_FALSE;        
            //throw scheme_exception(L"Number out of range");
        }
    }

    offset += digits.size();
    if (offset >= s.size()) {
	// Done. No . or e
	// We have an int or a bigint
        return SchemeObject::createIntegerNumber(sign*t);
    }
    
    if (s[offset] == L'/') {
        offset++;
        string denominator = extractDigits(s, offset, radix);
        if (denominator.size() == 0) {
            return S_FALSE;        
        }
        errno = 0;
        long d = strtol(denominator.c_str(), NULL, radix);
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
	    rational_type rational(sign*t, d);
            return SchemeObject::createRationalNumber(rational.normalized());
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
        errno = 0;    
        long f = strtol(fraction.c_str(), NULL, 10);
        if (errno == ERANGE) {
            // TODO: Clip fraction to the possible precision.        
            // throw scheme_exception(L"Number out of range"); 
            return S_FALSE;
                   
        }
        df = double(f) / pow(10.0, double(fraction.size()));
        offset += fraction.size();
    }
    if (offset >= s.size()) {
	// Done. No e
        return SchemeObject::createRealNumber(sign*(t + df));
    }
    
    long e = 0;
    if (radix == 10 && (s[offset] == L'e' || s[offset] == L's' ||
                        s[offset] == L'f' || s[offset] == L'd' || s[offset] == L'l')) {
        offset++;
        long esign = 1;
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
            //throw scheme_exception(L"Number out of range");        
        }
        offset += exponent.size();
    }
    if (offset >= s.size()) {
        return SchemeObject::createRealNumber(sign*(t + df) * pow(10,double(e)));
    }
    
    if (s[offset] == L'i' && (digits.size() != 0 || fraction.size() != 0) && sign_found) {
        offset++;    
        if (offset >= s.size()) {
            std::complex<double> z(0, sign*(t + df) * pow(10,double(e)));    
            return SchemeObject::createComplexNumber(z);    
        } else {
            return S_FALSE;        
        }
    }
    
    if (digits.size() != 0 || fraction.size() != 0) {
        SchemeObject* imag = i_string_2_number(s, radix, offset);
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
        // Guile uses precision 15. We'll do the same.
        // Comparing (* 4 (atan 1)) to the real digits of pi
        // it looks like the precision of double is about 15 
        // or 16 decimal digits.     
        ss << std::setbase(radix);
       	ss << std::setprecision(15);
        ss << d;
        double i;
        // Append ".0" if d contains no decimal point.
        if (::modf(d,&i) == 0.0) {
            ss << L".0";        
        }
        return ss.str();
    } else if (type == SchemeObject::COMPLEX_NUMBER) {
        double imag = o->imag->realValue();
        if (imag == 0.0) {
            return i_number_2_string(o->real, radix);        
        } else {
            ss << i_number_2_string(o->real, radix);
            if (o->imag->realValue() >= 0.0) {
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
    scheme->assign(L"<"                     ,0,0,1, (SchemeObject* (*)()) s_less, envt);
    scheme->assign(L">"                     ,0,0,1, (SchemeObject* (*)()) s_greater, envt);
    scheme->assign(L"<="                    ,0,0,1, (SchemeObject* (*)()) s_less_equal, envt);
    scheme->assign(L">="                    ,0,0,1, (SchemeObject* (*)()) s_greater_equal, envt);
    scheme->assign(L"="                     ,0,0,1, (SchemeObject* (*)()) s_equal, envt);
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
    scheme->assign(L"round"                 ,1,0,0, (SchemeObject* (*)()) s_round, envt);
    scheme->assign(L"ceiling"               ,1,0,0, (SchemeObject* (*)()) s_ceiling, envt);
    scheme->assign(L"floor"                 ,1,0,0, (SchemeObject* (*)()) s_floor, envt);
    scheme->assign(L"truncate"              ,1,0,0, (SchemeObject* (*)()) s_truncate, envt);
    scheme->assign(L"quotient"              ,2,0,0, (SchemeObject* (*)()) s_quotient, envt);
    scheme->assign(L"remainder"             ,2,0,0, (SchemeObject* (*)()) s_remainder, envt);
    scheme->assign(L"modulo"                ,2,0,0, (SchemeObject* (*)()) s_modulo, envt);
    scheme->assign(L"min"                   ,1,0,1, (SchemeObject* (*)()) s_min, envt);
    scheme->assign(L"max"                   ,1,0,1, (SchemeObject* (*)()) s_max, envt);
    scheme->assign(L"gcd"                   ,0,0,1, (SchemeObject* (*)()) s_gcd, envt);
    scheme->assign(L"lcm"                   ,0,0,1, (SchemeObject* (*)()) s_lcm, envt);
    scheme->assign(L"numerator"             ,1,0,0, (SchemeObject* (*)()) s_numerator, envt);
    scheme->assign(L"denominator"           ,1,0,0, (SchemeObject* (*)()) s_denominator, envt);
    scheme->assign(L"exact->inexact"        ,1,0,0, (SchemeObject* (*)()) s_exact_2_inexact, envt);
    scheme->assign(L"inexact->exact"        ,1,0,0, (SchemeObject* (*)()) s_inexact_2_exact, envt);
    scheme->assign(L"even?"                 ,1,0,0, (SchemeObject* (*)()) s_even_p, envt);
    scheme->assign(L"odd?"                  ,1,0,0, (SchemeObject* (*)()) s_odd_p, envt);
    scheme->assign(L"zero?"                 ,1,0,0, (SchemeObject* (*)()) s_zero_p, envt);
    scheme->assign(L"negative?"             ,1,0,0, (SchemeObject* (*)()) s_negative_p, envt);
    scheme->assign(L"positive?"             ,1,0,0, (SchemeObject* (*)()) s_positive_p, envt);
    scheme->assign(L"make-rectangular"      ,2,0,0, (SchemeObject* (*)()) s_make_rectangular, envt);
    scheme->assign(L"make-polar"            ,2,0,0, (SchemeObject* (*)()) s_make_polar, envt);
    scheme->assign(L"real-part"             ,1,0,0, (SchemeObject* (*)()) s_real_part, envt);
    scheme->assign(L"imag-part"             ,1,0,0, (SchemeObject* (*)()) s_imag_part, envt);
    scheme->assign(L"magnitude"             ,1,0,0, (SchemeObject* (*)()) s_magnitude, envt);
    scheme->assign(L"angle"                 ,1,0,0, (SchemeObject* (*)()) s_angle, envt);
}
