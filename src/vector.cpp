/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Copyright 2003 by Jesper Christensen <jesper@kalliope.org>
 *
 * Added Thu Apr 17 2003
 */

#include "vector.h"
#include <math.h>
#include <cassert>
#include <iostream>
#include "constants.h"

using namespace std;


Vector::~Vector() {
}

void Vector::normalize() {
    double s = length();

    if (IS_ZERO(s))
	return;

    double d = double(1.0)/s;
    scale(d);
}

void Vector::scale(float s) {
    _vector[0] *= s;
    _vector[1] *= s;
    _vector[2] *= s;
}

double Vector::norm() const {
    return _vector[0]*_vector[0] + _vector[1]*_vector[1] + _vector[2]*_vector[2];
}

double Vector::length() const {
    return sqrt(norm());
}

const Vector Vector::reflect(const Vector& N) {
    Vector L = *(this);
    Vector R = (2 * (N * L)) * N - L;
    return R;
}

ostream & operator<<(ostream &os, const Vector &x) {
    os << '(' << x[0] << ',';
    os << x[1] << ',';
    os << x[2] << ')';

    return os;
}


Vector Vector::operator/(const double x) const {
    assert(x != 0.0);
    const double inv = 1.0/x;
    return (*this)*inv;
}

bool Vector::operator==(const Vector& x) const {
    return IS_EQUAL(x[0],_vector[0]) &&
	   IS_EQUAL(x[1],_vector[1]) &&
           IS_EQUAL(x[2],_vector[2]);
}

Vector Vector::xProduct(const Vector& v1, const Vector& v2) {
    return Vector(v1[1]*v2[2] - v1[2]*v2[1],
	          v1[2]*v2[0] - v1[0]*v2[2],
   	          v1[0]*v2[1] - v1[1]*v2[0]);
}
//----------------------------------------
// Friends 
// ---------------------------------------

Vector operator*(const double x, const Vector &v) {
    return v*x;
}


