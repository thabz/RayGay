/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef MATRIX3_H
#define MATRIX3_H

#include "math/vector.h"
#include <iosfwd>

/// Some basic linear algebra
class Matrix3 {

    friend std::ostream &operator<<(std::ostream &os,const Matrix3 &m);

    public:
        /// The default constructor creates an identity
	Matrix3();

        /// Reset matrix to the identity
	void identity();

        /// Same as reset()
	void reset();

	/// Fills matrix with 0's
	void clear();
	
	/// Says whether this matrix is the identity
	bool isIdentity() const;

	/// Says whether this matrix is orthogonal
	bool isOrthogonal() const;
	
        /// Returns the inverse to this matrix
	Matrix3 inverse() const;

	/// Returns the transpose of the 3x3 matrix part
	Matrix3 transpose() const;

	/// Returns a matrix with the translation part stripped
	Matrix3 extractRotation() const;

	/// Multiply this matrix with a vector.
	Vector operator*(const Vector & v) const;

	/// Multiply with another Matrix
	Matrix3 operator*(const Matrix3 &) const;
	
	/// Multiply with another Matrix
	Matrix3 &operator*=(const Matrix3 &);

	/// Comparator
	bool operator==(const Matrix3 &m) const;

	/// Comparator
	bool operator!=(const Matrix3 &m) const;

	/// Rotate angle degrees around axis
	static Matrix3 matrixRotate(const Vector axis, const double angle);

	/// Rotate around the three axises
	static Matrix3 matrixRotate(const Vector angles);

	/// Translate along a vector
	static Matrix3 matrixTranslate(const Vector trans);
	
	/// Scale
	static Matrix3 matrixScale(const Vector& scale);

	/// Rotate v onto the positive z-axis
	static Matrix3 matrixOrient(const Vector& v);

	/// Orientation transformation matrix
	static Matrix3 matrixOrient(const Vector &x,const Vector &y,const Vector &z);
	
	/// Orientation transformation matrix
        static Matrix3 matrixOrient(const Vector &direction,const Vector &up);

	inline void set(const int col,const int row,const double val) {
	    _matrix[col*3+row] = val;
	}

    private:
	double _matrix[9];
	static double _identity[9];

	inline double get(const int col,const int row) const {
	    return _matrix[col*3+row];
	}

	inline double &element(const int col,const int row) {
	    return _matrix[col*3+row];
	}
};

inline
Vector Matrix3::operator*(const Vector &v) const {

    double prod[3];

    prod[0] = _matrix[0]*v[0] + _matrix[3]*v[1] + _matrix[6]*v[2];
    prod[1] = _matrix[1]*v[0] + _matrix[4]*v[1] + _matrix[7]*v[2];
    prod[2] = _matrix[2]*v[0] + _matrix[5]*v[1] + _matrix[8]*v[2];

    return Vector(prod[0],prod[1],prod[2]);
}

#endif
