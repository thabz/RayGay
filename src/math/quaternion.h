
#ifndef MATH_QUATERNION_H
#define MATH_QUATERNION_H

#include <iostream>
#include <cassert>
#include "math/vector.h"
#include "math/matrix.h"

/**
 * A class representing Quaternions.
 *
 * @see http://mathworld.wolfram.com/Quaternion.html
 */
class Quaternion 
{
    friend std::ostream& operator<<(std::ostream &os, const Quaternion& q);

    public:
   	/// Default constructor
        Quaternion();
	/// Constructor
	Quaternion(double a, double b, double c, double d);
	/// Constructor
	Quaternion(double s, const Vector& v);
	Quaternion operator+(const Quaternion& b) const;
	Quaternion operator*(const Quaternion& b) const;
	/// Scale quaternion
	Quaternion operator*(double b) const;
	/// Scale quaternion
	Quaternion operator/(double b) const;
	/// Test for equality
	bool operator==(const Quaternion& b) const;
	/// Test for inequality
	bool operator!=(const Quaternion& b) const;
	Quaternion conjugate() const;
	Quaternion inverse() const;
	double norm() const;
	double norm_squared() const;
	Vector rotate(const Vector& v) const;
	static Quaternion rotation(const Vector& v, double angle);
	Matrix toMatrix() const;

    private:
	double a1, a2, a3, a4;

};

inline
Quaternion::Quaternion() 
{
}

inline
Quaternion::Quaternion(double a, double b, double c, double d) 
{
    a1 = a;
    a2 = b;
    a3 = c;
    a4 = d;
}

inline
Quaternion::Quaternion(double s, const Vector& v) 
{
    a1 = s;
    a2 = v[0];
    a3 = v[1];
    a4 = v[2];
}

/**
 * Add two quaternions.
 */
inline
Quaternion Quaternion::operator+(const Quaternion& b) const 
{
    return Quaternion(a1 + b.a1, a2 + b.a2, a3 + b.a3, a4 + b.a4);
}

inline
Quaternion Quaternion::operator*(double d) const 
{
    return Quaternion(a1 * d, a2 * d, a3 * d, a4 * d);
}

inline
Quaternion Quaternion::operator/(double d) const 
{
    return Quaternion(a1 / d, a2 / d, a3 / d, a4 / d);
}


/**
 * Multiplication operator. Note that multiplication of 
 * quaternions is non-commutative, that is \f$ ab \f$ is not 
 * necessarily equal to \f$ ba \f$.
 */
inline
Quaternion Quaternion::operator*(const Quaternion& b) const 
{
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

bool Quaternion::operator==(const Quaternion& b) const 
{
    return IS_EQUAL(a1, b.a1) && 
           IS_EQUAL(a2, b.a2) && 
           IS_EQUAL(a3, b.a3) && 
           IS_EQUAL(a4, b.a4);
}

bool Quaternion::operator!=(const Quaternion& b) const 
{
    return !(*this == b);
}

/**
 * The quaternion conjugate. This is defined as 
 * \f$ \bar{a} = a_1 - a_2i - a_3j - a_4k \f$.
 */
inline
Quaternion Quaternion::conjugate() const 
{
    return Quaternion(a1, -a2, -a3, -a4);
}

/**
 * The squared norm of a quaternion. This is defined as
 * \f$ |a|^2 = a \bar{a} = \bar{a} a \f$
 */
inline
double Quaternion::norm_squared() const 
{
    return a1*a1 + a2*a2 + a3*a3 + a4*a4;
}

/**
 * The norm of a quaternion. This is defined as
 * \f$ |a| = \sqrt{a \bar{a}} = \sqrt{ \bar{a} a} \f$
 */
inline
double Quaternion::norm() const 
{
    return sqrt(a1*a1 + a2*a2 + a3*a3 + a4*a4);
}

/**
 * The inverse of a quaternion. This is 
 *
 * \f[ q^{-1} = \frac { \bar{q}} {|q|^2} \f]
 *
 * since \f$ q \bar{q} = \bar{q} q = |q|^2 \f$ from the
 * definition of norm and thus
 * \f$ \frac {q \bar{q}} {|q|^2} = \frac {\bar{q} q} {|q|^2} = 1 \f$.
 */
inline
Quaternion Quaternion::inverse() const 
{
    return this->conjugate() / this->norm_squared();
}


/**
 * Make a quaternion representing a rotation. This is simply
 *
 * \f[ q = (s,v) = (\cos(\frac{\theta}{2}), v \sin(\frac{\theta}{2})) \f]
 *
 * @param v a unit vector
 * @param angle the angle to rotate in degrees
 */
inline
Quaternion Quaternion::rotation(const Vector& v, double angle) 
{
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
inline
Vector Quaternion::rotate(const Vector& P) const 
{
    Quaternion p = Quaternion(0,P);
    Quaternion r = *this * p * this->conjugate();
    return Vector(r.a2, r.a3, r.a4);
}

/**
 * Converts this quaternion to the corresponding orthogonal rotation matrix. 
 * The quaternion must have norm = 1. The resulting matrix for a 
 * quaternion \f$ q = (a,b,c,d) \f$ when \f$ |q| = 1 \f$ is 
 *
 * \f[ \left[ \begin{array}{ccc}
  a^2 + b^2 - c^2 - d^2 & 2bc - 2ad & 2ac + 2bd \\
  2ad + 2bc & a^2 - b^2 + c^2 - d^2 & 2cd - 2ab \\
  2bd - 2ac & 2ab + 2cd & a^2 - b^2 - c^2 + d^2 \end{array} \right] \f]
 *
 * @see http://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
 */
Matrix Quaternion::toMatrix() const 
{
    assert(IS_EQUAL(norm(),1.0));
    Matrix m;
    double a = a1;
    double b = a2;
    double c = a3;
    double d = a4;
    double aa = a * a; double bb = b * b; double cc = c * c; double dd = d * d;
    double b2 = 2*b; double d2 = 2*d;
    double bc2 = b2*c; double ad2 = d2*a; double ac2 = 2*a*c;
    double bd2 = b2*d; double cd2 = d2*c; double ab2 = b2*a;
    m.set(0,0, aa + bb - cc - dd);
    m.set(1,0, bc2 - ad2);
    m.set(2,0, ac2 - bd2);
    m.set(0,1, ad2 + bc2);
    m.set(1,1, aa - bb + cc - dd);
    m.set(2,1, cd2 - ab2);
    m.set(0,2, bd2 - ac2);
    m.set(1,2, ab2 + cd2);
    m.set(2,2, aa - bb - cc + dd);
    return m;
}

std::ostream& operator<<(std::ostream& os, const Quaternion& q) 
{
    return os << "(" << q.a1 << "," << q.a2 << "," << q.a3 << "," << q.a4 << ")";
}

#endif

