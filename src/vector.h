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

/// Implements a vector

class Vector {
    friend std::ostream & operator<< (std::ostream &os, const Vector &x);
    friend Vector operator/(const Vector &v, const double x);
    friend Vector operator*(const double x, const Vector &v);
    friend Vector operator*(const Vector &v, const double x);
    friend Vector operator-(const Vector &v1, const Vector &v2);
    friend Vector operator+(const Vector &v1, const Vector &v2);


public:
    Vector(); ///< Constructor that returns a (0,0,0) vector
    Vector(double x, double y, double z); ///< Constructor
    ~Vector();
    void normalize(); ///< Normalize vector so that |v| = 1
    void scale(float s); ///< Scale the vector
    double norm() const; ///< Returns squared length of vector
    double length(); ///< Returns length of vector
    const Vector reflect(const Vector& norm); ///< Returns this vector reflected around another (both must be unit-vectors)

    double &operator[](const int i); ///< Index into coordinates
    const double &operator[](const int i) const; ///< Index into coordinates
    
    double operator*(const Vector &v) const; ///< Vector dot product (aka scalar product)

    bool operator==(const Vector &v) const;

    void test();
    
protected:
    double _vector[3]; ///< The x,y,z components of the vector
};

#endif
