
#include "math/sturmsequence.h"

SturmSequence::SturmSequence(const Polynomial& polynomial) {
    f.push_back(polynomial);
    f.push_back(polynomial.derivative());
    if (f[1].order() != 0) {
	uint n = 2;
	Polynomial remainder;
	do {
	    f[n-2].division(f[n-1],remainder);
	    remainder = remainder * -1;
	    f.push_back(remainder);
	    n++;
	} while (remainder.order() > 0 && !IS_ZERO(remainder.coefficient(0)));
    };
    
}

void SturmSequence::eval(double x, double* dest) const {
    uint num = f.size();
    for(uint i = 0; i < num; i++) {
	dest[i] = f[i].eval(x);
    }
}

#define sign(x) ((x >= 0) ? 1 : -1)

uint SturmSequence::signChanges(double x) const {
    uint result = 0;
    uint num = f.size();
    double* values = (double*) alloca(sizeof(double) * num);
    eval(x,values);
    int cur_sign = sign(values[0]);
    for(uint i = 1; i < num; i++) {
	if (sign(values[i]) != cur_sign) {
	    cur_sign = sign(values[i]);
	    result++;
	}
    }
    return result;
}

/**
 * Calculate number of distinct real roots in the interval \f$ ]a,b[ \f$.
 * a and b must not be roots of the polynomial.
 */
uint SturmSequence::rootCount(double a, double b) const {
    uint changes_a = signChanges(a);
    uint changes_b = signChanges(b);
    return changes_b - changes_a;
}

