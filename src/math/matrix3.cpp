/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include <string> 
#include <iostream>
#include <cassert>

#include "math/matrix3.h"
#include "math/vector.h"
#include "math/constants.h"

using namespace std;

double Matrix3::_identity[9] = {
    1.0, 0.0, 0.0,
    0.0, 1.0, 0.0,
    0.0, 0.0, 1.0
};

 
Matrix3::Matrix3(){
    reset();
}

void Matrix3::reset() {
    memcpy(_matrix,_identity,9*sizeof(double));
}

void Matrix3::identity() {
    reset();
}

void Matrix3::clear() {
    for(uint32_t i = 0; i < 9; i++) {
	_matrix[i] = 0;
    }
}

bool Matrix3::isIdentity() const {
    for(uint32_t i = 0; i < 9; i++) {
	if (!IS_EQUAL(_matrix[i],_identity[i]))
	    return false;
    }
    return true;
}

/**
 * Says whether this matrix is orthogonal.
 *
 * A matrix is orthogonal if its transpose is its inverse.
 * That is if \f$ M^TM = I \f$.
 */ 
bool Matrix3::isOrthogonal() const {
    Matrix3 transposed = transpose();
    Matrix3 prod = *this * transposed;
    return prod.isIdentity();
}

/// Multiply matrix with matrix 
Matrix3 Matrix3::operator*(const Matrix3 &m) const {

    Matrix3 prod;

    for (int c=0;c<3;c++) {
	for (int r=0;r<3;r++) {
	    prod.set(c,r,
		    get(c,0)*m.get(0,r) +
		    get(c,1)*m.get(1,r) +
		    get(c,2)*m.get(2,r));
	}
    }
    return prod;
}

/// Multiply matrix with matrix 
Matrix3 & Matrix3::operator*=(const Matrix3 &m) {
    return (*this) = (*this)*m;
}

ostream & operator<<(ostream &os,const Matrix3 &m) {
    for (int r = 0; r < 3; r++)
	for (int c = 0; c < 3; c++)
	    os << ' ' << m.get(c,r) << ( c == 2 ? '\n' : '\t');

    return os;
}

bool Matrix3::operator==(const Matrix3& m) const {
    for (int i = 0; i < 9; i++) {
	if (IS_NEQUAL(_matrix[i],m._matrix[i])) {
	    return false;
	}
    }
    return true;
}

bool Matrix3::operator!=(const Matrix3& m) const {
    for (int i = 0; i < 16; i++) {
	if (IS_NEQUAL(_matrix[i],m._matrix[i])) {
	    return true;
	}
    }
    return false;
}

/**
 * Create a transformation matrix that places v in the positive z-axis
 */ 
Matrix3 Matrix3::matrixOrient(const Vector& v) {
    Matrix3 r;
    double D = v.length();
    double a = v[0] / D; 
    double b = v[1] / D; 
    double c = v[2] / D; 
    double d = sqrt(b*b + c*c);
    if (IS_ZERO(v[1]) && IS_ZERO(v[2])) {
	// d == 0, so rotate +90 degrees around y-axis
	r.set(0,0,0);
	r.set(2,0,-a);
	r.set(0,2,a);
	r.set(2,2,0);
    } else {
	r.set(0,0,d),
	r.set(1,0,-a*b/d);
	r.set(2,0,-a*c/d);
	r.set(1,1,c/d);
	r.set(2,1,-b/d);
	r.set(0,2,a),
	r.set(1,2,b);
	r.set(2,2,c);
    }
    return  r;
}

/*! 
  \brief	Orientation transformation matrix
  \ingroup	Math
  \param      	x	New orientation for +x
  \param	y	New orientation for +y
  \param	z	New orientation for +z
  */
Matrix3 Matrix3::matrixOrient(const Vector &x,const Vector &y,const Vector &z)
{
    Matrix3 orient;

    orient.set(0,0,x.x());
    orient.set(0,1,x.y());
    orient.set(0,2,x.z());

    orient.set(1,0,y.x());
    orient.set(1,1,y.y());
    orient.set(1,2,y.z());

    orient.set(2,0,z.x());
    orient.set(2,1,z.y());
    orient.set(2,2,z.z());

    return orient;
}

/*! 
  \brief     Orientation transformation matrix
  \ingroup   Math
  \param	   direction	New orientation for +z
  \param     up           New orientation for +y
*/
Matrix3 Matrix3::matrixOrient(const Vector &direction,const Vector &up)
{
	assert(direction.norm()>0.0);
	assert(up.norm()>0.0);

	Vector d(direction);
	d.normalize();

	Vector u(up);
	u.normalize();

	return matrixOrient(Vector::xProduct(u,d),u,d);
}

/**
 * Create a rotation transformation
 *
 * @param angles The coordinates are the angles to rotate around each of the three axis
 */
Matrix3 Matrix3::matrixRotate(const Vector angles) {
    Matrix3 x = matrixRotate(Vector(1,0,0),angles[0]);
    Matrix3 y = matrixRotate(Vector(0,1,0),angles[1]);
    Matrix3 z = matrixRotate(Vector(0,0,1),angles[2]);
    return x * y * z;
}

/**
 * Create a scaling transformation.
 *
 * @param c the x,y,z scaling coefficents
 */
Matrix3 Matrix3::matrixScale(const Vector& c) {
    Matrix3 result;
    result.set(0,0,c[0]);
    result.set(1,1,c[1]);
    result.set(2,2,c[2]);
    return result;
}

/**
 * Create a rotation transformation.
 *
 * Using Rodrigues' rotation formula.
 * 
 * @see http://mathworld.wolfram.com/RodriguesRotationFormula.html
 */
Matrix3 Matrix3::matrixRotate(const Vector axis, const double angle) {
	Matrix3 rotate;

	double s = sin(angle*M_PI_DEG);
	double c = cos(angle*M_PI_DEG);
	double t = 1 - c;

	Vector ax = axis/sqrt(axis.norm());

	double x = ax[0];
	double y = ax[1];
	double z = ax[2];

	rotate.set(0,0,t*x*x+c);
	rotate.set(1,0,t*y*x+s*z);
	rotate.set(2,0,t*z*x-s*y);

	rotate.set(0,1,t*x*y-s*z);
	rotate.set(1,1,t*y*y+c);
	rotate.set(2,1,t*z*y+s*x);

	rotate.set(0,2,t*x*z+s*y);
	rotate.set(1,2,t*y*z-s*x);
	rotate.set(2,2,t*z*z+c);

	return rotate;
}

Matrix3 Matrix3::transpose() const {
    Matrix3 transposed;
    for(int r = 0; r < 3; r++) {
        for(int c = 0; c < 3; c++) {
	    transposed.set(r,c,get(c,r));
	}
    }
    return transposed;
}

Matrix3 Matrix3::inverse() const {
    Matrix3 inv;
    const double *m = _matrix;
    double *out = inv._matrix;
/* NB. OpenGL Matrices are COLUMN major. */
#define MAT(m,r,c) (m)[(c)*3+(r)]

/* Here's some shorthand converting standard (row,column) to index. */
#define m11 MAT(m,0,0)
#define m12 MAT(m,0,1)
#define m13 MAT(m,0,2)
#define m21 MAT(m,1,0)
#define m22 MAT(m,1,1)
#define m23 MAT(m,1,2)
#define m31 MAT(m,2,0)
#define m32 MAT(m,2,1)
#define m33 MAT(m,2,2)

   register double det;
   double tmp[9]; /* Allow out == in. */

   /* Inverse = adjoint / det. (See linear algebra texts.)*/

   tmp[0]= m22 * m33 - m23 * m32;
   tmp[1]= m23 * m31 - m21 * m33;
   tmp[2]= m21 * m32 - m22 * m31;

   /* Compute determinant as early as possible using these cofactors. */
   det= m11 * tmp[0] + m12 * tmp[1] + m13 * tmp[2];

   /* Run singularity test. */
   if (det == 0.0) {
      /* printf("invert_matrix: Warning: Singular matrix.\n"); */
      memcpy( out, _identity, 9*sizeof(double) );
      return inv;
   } else {
      double d12, d13, d23;
      register double im11, im12, im13;

      det = 1. / det;

      /* Compute rest of inverse. */
      tmp[0] *= det;
      tmp[1] *= det;
      tmp[2] *= det;
      tmp[3]  = 0.;

      im11 = m11 * det;
      im12 = m12 * det;
      im13 = m13 * det;
      tmp[3] = im13 * m32 - im12 * m33;
      tmp[4] = im11 * m33 - im13 * m31;
      tmp[5] = im12 * m31 - im11 * m32;
      tmp[7] = 0.;

      /* Pre-compute 2x2 dets for first two rows when computing */
      /* cofactors of last two rows. */
      d12 = im11*m22 - m21*im12;
      d13 = im11*m23 - m21*im13;
      d23 = im12*m23 - m22*im13;

      tmp[6] =  d23;
      tmp[7] = -d13;
      tmp[8] = d12;

      memcpy(out, tmp, 9*sizeof(double));
  }

#undef m11
#undef m12
#undef m13
#undef m21
#undef m22
#undef m23
#undef m31
#undef m32
#undef m33
#undef MAT

    return inv;
}

