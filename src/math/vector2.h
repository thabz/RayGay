
#ifndef VECTOR2_H
#define VECTOR2_H

#include "math/constants.h"

class Vector2 {
    public:
	Vector2();
	Vector2(Float x, Float y);

	Float &operator[](const int i); ///< Index into coordinates
	const Float &operator[](const int i) const; ///< Index into coordinates
	Vector2 operator*(const Float x) const;
	Vector2 operator+(const Vector2 &v) const;
	bool operator==(const Vector2 &v) const; ///< Comparator

	bool operator()(const Vector2* v1, const Vector2* v2) const {
	    return (*v1) == (*v2);
	}

    protected:
	Float _vector[2];
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
Vector2 Vector2::operator*(const Float x) const {
    return Vector2( x*_vector[0], x*_vector[1]);
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

#endif /* VECTOR2_H */
