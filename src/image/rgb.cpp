
#include <iostream>
#include "rgb.h"

using namespace std;

void RGB::clip() {
    for (int i=0; i<3; i++) {
	if (_vector[i] < 0)
	    _vector[i] = 0;
	if (_vector[i] > 1)
	    _vector[i] = 1;
    }
}

double RGB::sqrDistance(const RGB& other) const {
    return (*this-other).norm();
}


ostream & operator<<(ostream &os, const RGB &x) {
    os << '(' << x[0] << ',';
    os << x[1] << ',';
    os << x[2] << ')';

    return os;
}

RGB operator*(const double x, const RGB &v) {
    return v*x;
}
