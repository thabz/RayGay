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

#include "vector.h"
#include <iosfwd>

/// Some basic linear algebra
class Matrix {

    friend std::ostream &operator<<(std::ostream &os,const Matrix &m);

    public:
	Matrix();
	Matrix(const Matrix &matrix);
	~Matrix();

	void identity();
	void reset();
	
	/// Says whether this matrix is the identity
	bool isIdentity() const;
	
        /// Returns the inverse to this matrix
	Matrix inverse() const;

	Vector operator*(const Vector & v) const;
	Matrix operator*(const Matrix &) const;
	Matrix &operator*=(const Matrix &);

	/// Rotate angle degrees around axis
	static Matrix matrixRotate(const Vector axis,const double angle);
	/// Translate along a vector
	static Matrix matrixTranslate(const Vector trans);
	static void test();


    private:
	double _matrix[16];
	static double _identity[16];

	static void invertMatrixGeneral(const double *in, double *out);

	inline void set(const int col,const int row,const double val) {
	    _matrix[col*4+row] = val;
	}

	inline double get(const int col,const int row) const {
	    return _matrix[col*4+row];
	}

	inline double &element(const int col,const int row) {
	    return _matrix[col*4+row];
	}
};

#endif
