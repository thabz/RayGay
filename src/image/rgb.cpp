
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
//    return (*this-other).norm();
    return fabs(_vector[0]-other._vector[0])+
	   fabs(_vector[1]-other._vector[1])+
	   fabs(_vector[2]-other._vector[2]);
	   }


ostream & operator<<(ostream &os, const RGB &x) {
    os << '(' << x[0] << ',';
    os << x[1] << ',';
    os << x[2] << ')';

    return os;
}
