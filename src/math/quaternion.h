
#ifndef MATH_QUATERNION_H
#define MATH_QUATERNION_H

#include "math/vector.h"

/**
 * A class representing Quaternions.
 *
 * @see http://mathworld.wolfram.com/Quaternion.html
 */
class Quaternion 
{
    public:
	Quaternion(double a, double b, double c, double d);
	Quaternion(double s, const Vector& v);
	Quaternion operator+(const Quaternion& b) const;
	Quaternion operator*(const Quaternion& b) const;
	Quaternion conjugate() const;
	double norm() const;
	Vector rotate(const Vector& v) const;
	static Quaternion rotation(const Vector& v, double angle);

    private:
	double a1, a2, a3, a4;

};

Quaternion::Quaternion(double a, double b, double c, double d) {
    a1 = a;
    a2 = b;
    a3 = c;
    a4 = d;
}

Quaternion::Quaternion(double s, const Vector& v) {
    a1 = s;
    a2 = v[0];
    a3 = v[1];
    a4 = v[2];
}

Quaternion Quaternion::operator+(const Quaternion& b) const {
    return Quaternion(a1 + b.a1, a2 + b.a2, a3 + b.a3, a4 + b.a4);
}

/**
 * Multiplication operator. Note that multiplication of 
 * quaternions is non-commutative, that is \f$ ab \f$ is not 
 * necessarily equal to \f$ ba \f$.
 */
Quaternion Quaternion::operator*(const Quaternion& b) const {
    double b1 = b.a1;
    double b2 = b.a2;
    double b3 = b.a3;
    double b4 = b.a4;
    return Quaternion(
	    a1*b1 - a2*b2 - a3*b3 - a4*b4,
	    a1*b2 + a2*b1 + a3*b4 - a4*b3,
	    a1*b3 - a2*b4 + a3*b1 + a4*b2,
	    a1*b4 + a2*b3 - a3*b2 + a4*b1);
}

Quaternion Quaternion::conjugate() const {
    return Quaternion(a1, -a2, -a3, -a4);
}

double Quaternion::norm() const {
    return sqrt(a1*a1 + a2*a2 + a3*a3 + a4*a4);
}

/**
 * Make a quaternion representing a rotation.
 *
 * @param v a unit vector
 * @param angle the angle to rotate in degrees
 */
Quaternion Quaternion::rotation(const Vector& v, double angle) {
    double rad = DEG2RAD(angle);
    double sina = sin(0.5 * rad);
    double cosa = cos(0.5 * rad);
    return Quaternion(cosa, v * sina);
}

/**
 * Rotate a point. 
 * 
 * Let \f$ p = (0,P) \f$ be a quaternion, then the rotated point
 * is given as \f$ p' = q p \bar{q} \f$ where \f$ \bar{q} \f$ is
 * the conjugate of \f$ q \f$.
 * 
 * @param P the point to rotate
 */
Vector Quaternion::rotate(const Vector& P) const {
    Quaternion p = Quaternion(0,point);
    Quaternion r = *this * p * this->conjugate();
    return Vector(r.a2, r.a3, r.a4);
}

#endif

