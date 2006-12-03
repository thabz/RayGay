
#ifndef VECTOR2_H
#define VECTOR2_H

#include "math/constants.h"

/// Implements a 2D vector
class Vector2 
{
    friend Vector2 operator*(const double x, const Vector2 &v);
    
    public:
	/// Constructs the origin
	Vector2();

	/// Constructor
	Vector2(Float x, Float y);

	Float &operator[](const int i); ///< Index into coordinates
	const Float &operator[](const int i) const; ///< Index into coordinates

	/// Scale vector
	Vector2 operator*(const Float x) const;
	/// Divide by constant
	Vector2 operator/(const Float x) const; 

	/// Add two vectors
	Vector2 operator+(const Vector2 &v) const; 
	/// Subtract two vectors
	Vector2 operator-(const Vector2 &v) const; 
	bool operator==(const Vector2 &v) const; ///< Comparator
	bool operator!=(const Vector2& x) const; ///< Comparator

	/// Comparator
	bool operator()(const Vector2* v1, const Vector2* v2) const;

	/// Returns squared length of vector
	double norm() const;

	// Returns length of vector
	double length() const;

    protected:
	Float _vector[2]; ///< The x,y components of the vector
};

inline
Vector2::Vector2() {
    _vector[0] = _vector[1] = 0;
}

inline
Vector2::Vector2(Float x, Float y) {
    _vector[0] = x;
    _vector[1] = y;
}

inline
Vector2 Vector2::operator+(const Vector2 &v) const {
    return Vector2(v._vector[0] + _vector[0], v._vector[1] + _vector[1]);
}

inline
Vector2 Vector2::operator-(const Vector2 &v) const {
    return Vector2(_vector[0] - v._vector[0], _vector[1] - v._vector[1]);
}

inline
Vector2 Vector2::operator*(const Float x) const {
    return Vector2( x*_vector[0], x*_vector[1]);
}

inline
Vector2 operator*(const double x, const Vector2 &v) {
    return Vector2(v._vector[0]*x,v._vector[1]*x);
}

inline
Vector2 Vector2::operator/(const Float x) const {
    return Vector2(_vector[0] / x, _vector[1] / x);
}

inline
double &Vector2::operator[](const int i) {
    //assert(i>=0 && i<3);
    return _vector[i];
}

inline
const double &Vector2::operator[](const int i) const {
    //assert(i>=0 && i<3);
    return _vector[i];
}

inline 
double Vector2::norm() const {
    return _vector[0]*_vector[0] + _vector[1]*_vector[1];
}

inline 
double Vector2::length() const {
    return hypot(_vector[0],_vector[1]);
}

inline
bool Vector2::operator==(const Vector2& x) const {
    return IS_EQUAL(x[0],_vector[0]) &&
           IS_EQUAL(x[1],_vector[1]);
}

inline
bool Vector2::operator!=(const Vector2& x) const {
    return IS_NEQUAL(x[0],_vector[0]) ||
           IS_NEQUAL(x[1],_vector[1]);
}

inline
bool Vector2::operator()(const Vector2* v1, const Vector2* v2) const {
    return (*v1) == (*v2);
}

#endif /* VECTOR2_H */
