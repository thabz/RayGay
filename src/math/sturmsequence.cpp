
#include <iostream>
#include "math/sturmsequence.h"

SturmSequence::SturmSequence(const Polynomial& polynomial) {
    Polynomial pol0 = Polynomial(0.0);
    f.push_back(polynomial);
    f.push_back(polynomial.derivative());
    if (!(f[1] == pol0)) {
	uint n = 2;
	Polynomial remainder;
	do {
	    f[n-2].division(f[n-1],remainder);
	    remainder = remainder * -1;
	    std::cout << remainder << std::endl;
	    f.push_back(remainder);
	    n++;
	} while (!(remainder == pol0));
    };
    
}

void SturmSequence::eval(double x, double* dest) const {
    uint num = f.size();
    for(uint i = 0; i < num; i++) {
	dest[i] = f[i].eval(x);
    }
}

#define sign(x) ((x >= 0) ? 1 : -1)

int SturmSequence::signChanges(double x) const {
    uint num = f.size() - 1; // Ignore last polynomial that is constant 0.

    double values[num];
    eval(x,values);

    uint result = 0;
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
    return abs(signChanges(b) - signChanges(a));
}

