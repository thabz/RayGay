
#include "math/sturmsequence.h"

SturmSequence::SturmSequence(const Polynomial& polynomial) {
    f.push_back(polynomial);
    f.push_back(polynomial.derivative());
    // TODO: Create rest of sequence
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

uint SturmSequence::rootCount(double a, double b) const {
    uint changes_a = signChanges(a);
    uint changes_b = signChanges(b);
    return changes_b - changes_a;
}

