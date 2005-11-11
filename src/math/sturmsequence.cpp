
#include <iostream>
#include <cassert>
#include "math/sturmsequence.h"

SturmSequence::SturmSequence(const Polynomial& polynomial) {
    assert(f.size() == 0);
    Polynomial pol0 = Polynomial(0.0);
    f.push_back(polynomial);
    f.push_back(polynomial.derivative());
    if (!(f[1] == pol0)) {
	uint32_t n = 2;
	Polynomial remainder;
	do {
	    f[n-2].division(f[n-1],remainder);
	    remainder = remainder * -1;
	    f.push_back(remainder);
	    n++;
	} while (!(remainder == pol0));
    };
    
}

void SturmSequence::eval(double x, double* dest) const {
    uint32_t num = f.size();
    for(uint32_t i = 0; i < num; i++) {
	dest[i] = f[i].eval(x);
    }
}

int SturmSequence::signChanges(double x) const {
    uint32_t num = f.size() - 1; // Ignore last polynomial that is constant 0.

    double values[num];
    eval(x,values);

    uint32_t result = 0;
    for(uint32_t i = 1; i < num; i++) {
	if (!SAME_SIGN(values[i], values[i-1])) {
	    result++;
	}
    }
    return result;
}

/**
 * Calculate number of distinct real roots in the interval \f$ ]a,b[ \f$.
 * a and b must not be roots of the polynomial.
 */
uint32_t SturmSequence::rootCount(double a, double b) const {
    return abs(signChanges(b) - signChanges(a));
}

