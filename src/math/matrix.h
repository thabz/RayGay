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

#ifndef MATRIX_H
#define MATRIX_H

#include "math/vector.h"
#include <iosfwd>

/// Some basic linear algebra
class Matrix {

    friend std::ostream &operator<<(std::ostream &os,const Matrix &m);

    public:
        /// The default constructor creates an identity
	Matrix();

        /// Reset matrix to the identity
	void identity();

        /// Same as reset()
	void reset();

	/// Fills matrix with 0's
	void clear();
	
	/// Says whether this matrix is the identity
	bool isIdentity() const;
	
        /// Returns the inverse to this matrix
	Matrix inverse() const;

	/// Returns the transpose of the 3x3 matrix part
	Matrix transpose() const;

	/// Returns a matrix with the translation part stripped
	Matrix extractRotation() const;

	/// Multiply this matrix with a vector.
	Vector operator*(const Vector & v) const;

	/// Multiply with another Matrix
	Matrix operator*(const Matrix &) const;
	
	/// Multiply with another Matrix
	Matrix &operator*=(const Matrix &);

	/// Comparator
	bool operator==(const Matrix &m) const;

	/// Comparator
	bool operator!=(const Matrix &m) const;

	/// Rotate angle degrees around axis
	static Matrix matrixRotate(const Vector axis, const double angle);

	/// Rotate around the three axises
	static Matrix matrixRotate(const Vector angles);

	/// Translate along a vector
	static Matrix matrixTranslate(const Vector trans);
	
	/// Scale
	static Matrix matrixScale(const Vector& scale);

	/// Rotate v onto the positive z-axis
	static Matrix matrixOrient(const Vector& v);

	/// Orientation transformation matrix
	static Matrix matrixOrient(const Vector &x,const Vector &y,const Vector &z);
	
	/// Orientation transformation matrix
        static Matrix matrixOrient(const Vector &direction,const Vector &up);

	inline void set(const int col,const int row,const double val) {
	    _matrix[col*4+row] = val;
	}

    private:
	double _matrix[16];
	static double _identity[16];

	static void invertMatrixGeneral(const double *in, double *out);


	inline double get(const int col,const int row) const {
	    return _matrix[col*4+row];
	}

	inline double &element(const int col,const int row) {
	    return _matrix[col*4+row];
	}
};

#define MAT(m,r,c) (m)[(c)*4+(r)]
#define m11 MAT(m,0,0)
#define m12 MAT(m,0,1)
#define m13 MAT(m,0,2)
#define m14 MAT(m,0,3)
#define m21 MAT(m,1,0)
#define m22 MAT(m,1,1)
#define m23 MAT(m,1,2)
#define m24 MAT(m,1,3)
#define m31 MAT(m,2,0)
#define m32 MAT(m,2,1)
#define m33 MAT(m,2,2)
#define m34 MAT(m,2,3)
#define m41 MAT(m,3,0)
#define m42 MAT(m,3,1)
#define m43 MAT(m,3,2)
#define m44 MAT(m,3,3)

/**
 * Multiply this with a vector
 * 
 * \todo Can be optimized by unrolling loops.
 */
inline
Vector Matrix::operator*(const Vector &v) const {

    double prod[4] = { 0,0,0,0 };
    const double *m = _matrix;

    prod[0] = m11*v[0] + m12*v[1] + m13*v[2] + m14;
    prod[1] = m21*v[0] + m22*v[1] + m23*v[2] + m24;
    prod[2] = m31*v[0] + m32*v[1] + m33*v[2] + m34;
    prod[3] = m41*v[0] + m42*v[1] + m43*v[2] + m44;

    double div = 1.0 / prod[3];

    return Vector(prod[0]*div,prod[1]*div,prod[2]*div);
}

#endif
