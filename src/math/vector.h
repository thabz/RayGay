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

#ifndef VECTOR_H
#define VECTOR_H

#include <iosfwd>
#include "math/constants.h"

/// Implements a 3D vector
class Vector {
    friend std::ostream & operator<< (std::ostream &os, const Vector &x);
    friend Vector operator*(const double x, const Vector &v);


public:
    Vector() {}; ///< Constructor that returns a (0,0,0) vector
    Vector(double x, double y, double z); ///< Constructor
    void normalize(); ///< Normalize vector so that |v| = 1
    void scale(float s); ///< Scale the vector
    double norm() const; ///< Returns squared length of vector
    double length() const; ///< Returns length of vector
    const Vector reflect(const Vector& norm); ///< Returns this vector reflected around another (both must be unit-vectors)

    double &operator[](const int i); ///< Index into coordinates
    const double &operator[](const int i) const; ///< Index into coordinates
    double operator*(const Vector &v) const; ///< Vector dot product (aka scalar product)
    /// Vector addition
    Vector operator+(const Vector &v) const;
    /// Vector subtraction 
    Vector operator-(const Vector &v) const;
    /// Vector multiplication
    Vector operator*(const double x) const;
    /// Vector division
    Vector operator/(const double x) const;
    static Vector xProduct (const Vector& v1, const Vector& v2); ///< Returns the scalar product v1 x v2

    static double area(const Vector& v0, const Vector& v1, const Vector& v2);  ///< The area of the triangle with the vertices v0, v1 and v2
    bool operator()(const Vector* v1, const Vector* v2) const {
	return (*v1) == (*v2);
    }

    /// x-component of this vector
    double x() const { return _vector[0]; };
    /// y-component of this vector
    double y() const { return _vector[1]; };
    /// z-component of this vector
    double z() const { return _vector[2]; };

    /// Comparator
    bool operator==(const Vector &v) const;

protected:
    double _vector[3]; ///< The x,y,z components of the vector
};

inline
Vector Vector::operator+(const Vector &v) const {
    return Vector(v[0] + _vector[0], v[1] + _vector[1], v[2] + _vector[2]);
}

inline
Vector Vector::operator-(const Vector &v) const {
    return Vector( _vector[0] - v[0], _vector[1] - v[1], _vector[2] - v[2]);
}

inline
Vector Vector::operator*(const double x) const {
    return Vector( x*_vector[0], x*_vector[1], x*_vector[2]);
}

inline
double &Vector::operator[](const int i) {
    //assert(i>=0 && i<3);
    return _vector[i];
}

inline
const double &Vector::operator[](const int i) const {
    //assert(i>=0 && i<3);
    return _vector[i];
}

inline
double Vector::operator*(const Vector &x) const {
    return _vector[0]*x[0] + _vector[1]*x[1] + _vector[2]*x[2];
}

/*
inline
Vector::Vector() {
    _vector[0] = _vector[1] = _vector[2] = 0;
}
*/

inline
Vector::Vector(double x, double y, double z) {
    _vector[0] = x;
    _vector[1] = y;
    _vector[2] = z; 
}

#endif
