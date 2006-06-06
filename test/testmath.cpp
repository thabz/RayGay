
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <iostream>

#include "math/vector.h"
#include "math/vector2.h"
#include "math/matrix.h"
#include "math/matrix3.h"
#include "math/functions.h"
#include "math/rootfinder.h"
#include "math/polynomial.h"
#include "math/sturmsequence.h"
#include "math/quaternion.h"
#include "math/interval.h"
#include "math/poisson_disc.h"
#include "testing.h"

using namespace std;

class vector_test : public Test {

    public:
	void run() {
	    /*
	       cout << "Size of short: " << sizeof(short) << endl;
	       cout << "Size of int: " << sizeof(int) << endl;
	       cout << "Size of long: " << sizeof(long) << endl;
	       cout << "Size of float: " << sizeof(float) << endl;
	       cout << "Size of double: " << sizeof(double) << endl;
	       cout << "Size of long double: " << sizeof(long double) << endl;
	       cout << "Size of Vector: " << sizeof(Vector) << endl;
	       */

	    Vector v = Vector(1,1,1);
	    assertTrue(IS_EQUAL(v.norm(),3));
	    assertTrue(IS_EQUAL(v.norm(),3.0));
	    assertTrue(IS_EQUAL(v.length(),sqrt(3.0)));

	    v = v - v;
	    assertTrue(IS_ZERO(v.norm()));
	    assertTrue(IS_ZERO(v.length()));

	    v = v + v;
	    assertTrue(IS_ZERO(v.norm()));
	    assertTrue(IS_ZERO(v.length()));

	    v = v + Vector(1,2,3);
	    v.normalize();
	    assertTrue(IS_EQUAL(v.norm(),1));
	    assertTrue(IS_EQUAL(v.length(),1));

	    v = Vector(1,2,3);
	    v = 2 * v;
	    assertTrue(v == Vector(2,4,6));
	    assertTrue(v != Vector(2,4,7));

	    v = v / 2;
	    assertTrue(v == Vector(1,2,3));
	    assertTrue(v != Vector(1,2,1));

	    v.scale(10);
	    assertTrue(v == Vector(10,20,30));
	    assertTrue(v != Vector(1,2,3));

	    v.scale(-1);
	    assertTrue(v == Vector(-10,-20,-30));
	    assertTrue(v != Vector(2,2,2));

	    assertTrue(2 * v == v * 2);

	    Vector w = Vector(100,200,300);
	    assertTrue(w * v == v * w);

	    assertTrue(Vector::xProduct(v,w) == Vector::xProduct(w,v));

	    v = Vector(1,2,3);
	    assertEqualV(-v, Vector(-1,-2,-3));
	    assertEqualV(v + -v, Vector(0,0,0));

	    // Test polar coordinates
	    v = Vector(1,0,0);
	    w = v.toPolar();
	    assertTrue(IS_EQUAL(w[0],1));
	    assertTrue(!IS_EQUAL(w[1],0));
	    assertTrue(IS_EQUAL(w[2],0));

	    v = Vector(0,0,1);
	    w = v.toPolar();
	    assertTrue(IS_EQUAL(w[0],1));
	    assertTrue(IS_EQUAL(w[1],0));
	    assertTrue(IS_EQUAL(w[2],0));

	    v = Vector(1,1,1);
	    w = v.toPolar();
	    assertTrue(IS_EQUAL(w[0],sqrt(3.0)));
	    assertTrue(!IS_EQUAL(w[1],0));
	    assertTrue(!IS_EQUAL(w[2],0));

	    v = Vector(0,1,1);
	    w = v.toPolar();
	    assertTrue(IS_EQUAL(w[0],sqrt(2.0)));

	    assertTrue(Vector(10,10,10).toPolar().toRectangular() == Vector(10,10,10));
	    assertTrue(Vector(10,10,-10).toPolar().toRectangular() == Vector(10,10,-10));
	    assertTrue(Vector(10,-10,10).toPolar().toRectangular() == Vector(10,-10,10));
	    assertTrue(Vector(10,-10,-10).toPolar().toRectangular() == Vector(10,-10,-10));
	    assertTrue(Vector(-10,10,10).toPolar().toRectangular() == Vector(-10,10,10));
	    assertTrue(Vector(-10,10,-10).toPolar().toRectangular() == Vector(-10,10,-10));
	    assertTrue(Vector(-10,-10,10).toPolar().toRectangular() == Vector(-10,-10,10));
	    assertTrue(Vector(-10,-10,-10).toPolar().toRectangular() == Vector(-10,-10,-10));
	    assertTrue(Vector(0,10,10).toPolar().toRectangular() == Vector(0,10,10));

	    // Test += operator
	    v = Vector(1,2,3);
	    v += Vector(10,20,30);
	    assertTrue(v == Vector(11,22,33));

	    // Test -= operator
	    v = Vector(1,2,3);
	    v -= Vector(10,20,30);
	    assertTrue(v == Vector(-9,-18,-27));

	    // Test *= operator
	    v = Vector(1,2,3);
	    v *= 2.0;
	    assertTrue(v == Vector(2,4,6));

	    // Test norm
	    v = Vector(10,11,12);
	    w = Vector(20,31,42);
	    assertTrue(IS_EQUAL((v-w).norm(),(w-v).norm()));

	    // Test scalar
	    v = Vector(10,10,10);
	    v.normalize();
	    assertTrue(IS_EQUAL(v*v,1));

	    v = Vector(0,0,1);
	    assertTrue(IS_EQUAL(v*v,1));

	    // Test area()
	    assertTrue(0.5 == Vector::area(Vector(1,1,1), Vector(2,1,1), Vector(1,2,1)));
	    assertTrue(1.0 == Vector::area(Vector(0,0,0), Vector(0,2,0), Vector(1,0,0)));
	    assertTrue(2.0 == Vector::area(Vector(0,0,0), Vector(0,2,0), Vector(2,0,0)));
	}
};

class vector2_test : public Test {
    public:
	void run() {
	    Vector2 v = Vector2(1.0,2.0);
	    assertTrue(v[0] == 1);
	    assertTrue(v[1] == 2);
	    v =  v * 2.0;
	    Vector2 w = Vector2(2.0,4.0);
	    assertTrue(v == w);

	    v = Vector2(1.0,2.0);
	    w = Vector2(10.0,20.0);
	    assertTrue(w + v == Vector2(11,22));
	    assertTrue(w -v == Vector2(9,18));
	    assertTrue(w / 2.0 == Vector2(5,10));
	    assertTrue(w * 0.5 == Vector2(5,10));
	}
};

class matrix3_test : public Test {
    private:
	    Matrix3 id,res,op1,op2,op3;
    public:
	void run() {
	    /* Test inverse() */
	    op1 = Matrix3::matrixRotate(Vector(10,30,19),12);
	    assertTrue(op1.isOrthogonal());
	    assertTrue(op1.inverse().isOrthogonal());
	    assertFalse(op1.isIdentity());
	    res = op1*id*op1.inverse();
	    assertTrue(res.isIdentity());

	    op3 = op2.inverse();
	    op3 = op1*op2;
	    assertTrue(op3.isOrthogonal());
	    assertFalse(op3.isIdentity());
	    res = op3*id*op3.inverse();
	    assertTrue(res.isIdentity());

	    // Test == and !=
	    op1 = Matrix3::matrixRotate(Vector(401,221,39),40);
	    op2 = Matrix3::matrixRotate(Vector(401,221,39),-40);
	    assertTrue(op1.isOrthogonal());
	    assertTrue(op2.isOrthogonal());
	    assertTrue(op1 != op2);
	    assertTrue((op1*op2).isIdentity());
	    op2 = op2.inverse();
	    assertTrue(op2.isOrthogonal());
	    assertTrue(op1 == op2);

	    op2 = Matrix3::matrixRotate(Vector(401,221,39),-20);
	    op2 = op2 * Matrix3::matrixRotate(Vector(401,221,39),-20);
	    assertTrue(op1 != op2);
	    op2 = op2.inverse();
	    assertTrue(op1 == op2);

	    // Test matrixOrient(Vector,Vector)

	    Vector v = Vector(1,1,0);
	    op1 = Matrix3::matrixOrient(v,Vector(0,1,0));
	    op1 = op1.inverse();
	    v = op1 * v;
	    assertTrue(IS_ZERO(v[0]));
	    assertTrue(IS_ZERO(v[1]));

	    /// Test matrixOrient(Vector)
	    v = Vector(20,391,29);
	    op1 = Matrix3::matrixOrient(v);
	    Vector w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));

	    v = Vector(0,13,0);
	    op1 = Matrix3::matrixOrient(v);
	    w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));
	    assertTrue(IS_EQUAL(w[2],13));

	    v = Vector(0,4,3);
	    op1 = Matrix3::matrixOrient(v);
	    w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));
	    assertTrue(IS_EQUAL(w[2],5));

	    v = Vector(0,0,71);
	    op1 = Matrix3::matrixOrient(v);
	    w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));
	    assertTrue(IS_EQUAL(w[2],71));

	    v = Vector(19,0,0);
	    op1 = Matrix3::matrixOrient(v);
	    w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));
	    assertTrue(IS_EQUAL(w[2],19));

	    // Test matrix rotate
	    v = Vector(1,0,0);
	    op1 = Matrix3::matrixRotate(Vector(0,0,1),90);
	    assertTrue(op1.isOrthogonal());
	    assertTrue(op1*v == Vector(0,-1,0));
	    op1 = Matrix3::matrixRotate(Vector(0,0,1),180);
	    assertTrue(op1*v == Vector(-1,0,0));
	    op1 = Matrix3::matrixRotate(Vector(0,0,1),35);
	    op2 = Matrix3::matrixRotate(Vector(0,0,1),-35);
	    assertTrue(op1*op2*v == v);
	    assertTrue(op2*op1*v == v);
	    op1 = Matrix3::matrixRotate(Vector(3,4,6),360);
	    assertTrue(op1*v == v);
	    op1 = Matrix3::matrixRotate(Vector(3,4,6),180);
	    op2 = Matrix3::matrixRotate(Vector(3,4,6),180);
	    assertTrue((op1*op2).isOrthogonal());
	    assertTrue(op1*op2*v == v);

	    // Test matrix scale
	    v = Vector(3,4,5);
	    op1 = Matrix3::matrixScale(Vector(2,2,2));
	    assertFalse(op1.isOrthogonal());
	    assertFalse(op1.isIdentity());
	    assertTrue(op1*v == Vector(6,8,10));
	    op1 = Matrix3::matrixScale(Vector(5,6,7));
	    assertFalse(op1.isOrthogonal());
	    assertFalse(op1.isIdentity());
	    assertTrue(op1*v == Vector(15,24,35));
	    op1 = Matrix3::matrixScale(Vector(5,6,7));
	    op2 = Matrix3::matrixScale(Vector(1.0/5.0,1.0/6.0,1.0/7.0));
	    assertFalse(op1.isOrthogonal());
	    assertFalse(op1.isIdentity());
	    assertFalse(op2.isOrthogonal());
	    assertFalse(op2.isIdentity());
	    assertTrue(op1*op2*v == v);
	    assertTrue(op2*op1*v == v);
	    assertTrue((op1*op2).isIdentity());
	    assertTrue(op1.inverse() == op2);
	}
};

class matrix_test : public Test {
    public:
	void run() {
	    /* Test inverse() */
	    Matrix id,res;
	    Matrix op1 = Matrix::matrixRotate(Vector(10,30,19),12);
	    assertTrue(op1.isOrthogonal());
	    assertTrue(op1.inverse().isOrthogonal());
	    assertFalse(op1.isIdentity());
	    res = op1*id*op1.inverse();
	    assertTrue(res.isIdentity());

	    Matrix op2 = Matrix::matrixTranslate(Vector(401,221,39));
	    assertFalse(op2.isOrthogonal());
	    assertFalse(op2.isIdentity());
	    res = op2*id*op2.inverse();
	    assertTrue(res.isIdentity());

	    Matrix op3 = op1*op2;
	    assertFalse(op3.isOrthogonal());
	    assertFalse(op3.isIdentity());
	    res = op3*id*op3.inverse();
	    assertTrue(res.isIdentity());

	    // Test == and !=
	    op1.identity();
	    op2.identity();
	    assertTrue(op1 == op2);
	    op1 = Matrix::matrixTranslate(Vector(401,221,39));
	    op2 = Matrix::matrixTranslate(Vector(-401,-221,-39));
	    assertTrue(op1 != op2);
	    op2 = op2.inverse();
	    assertTrue(op1 == op2);

	    op1 = Matrix::matrixRotate(Vector(401,221,39),40);
	    op2 = Matrix::matrixRotate(Vector(401,221,39),-40);
	    assertTrue(op1.isOrthogonal());
	    assertTrue(op2.isOrthogonal());
	    assertTrue(op1 != op2);
	    op2 = op2.inverse();
	    assertTrue(op2.isOrthogonal());
	    assertTrue(op1 == op2);

	    op2 = Matrix::matrixRotate(Vector(401,221,39),-20);
	    op2 = op2 * Matrix::matrixRotate(Vector(401,221,39),-20);
	    assertTrue(op1 != op2);
	    op2 = op2.inverse();
	    assertTrue(op1 == op2);

	    // Test matrixOrient(Vector,Vector)

	    Vector v = Vector(1,1,0);
	    op1 = Matrix::matrixOrient(v,Vector(0,1,0));
	    op1 = op1.inverse();
	    v = op1 * v;
	    assertTrue(IS_ZERO(v[0]));
	    assertTrue(IS_ZERO(v[1]));

	    /// Test matrixOrient(Vector)
	    v = Vector(20,391,29);
	    op1 = Matrix::matrixOrient(v);
	    Vector w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));

	    v = Vector(0,13,0);
	    op1 = Matrix::matrixOrient(v);
	    w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));
	    assertTrue(IS_EQUAL(w[2],13));

	    v = Vector(0,4,3);
	    op1 = Matrix::matrixOrient(v);
	    w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));
	    assertTrue(IS_EQUAL(w[2],5));

	    v = Vector(0,0,71);
	    op1 = Matrix::matrixOrient(v);
	    w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));
	    assertTrue(IS_EQUAL(w[2],71));

	    v = Vector(19,0,0);
	    op1 = Matrix::matrixOrient(v);
	    w = op1 * v;
	    assertTrue(IS_ZERO(w[0]));
	    assertTrue(IS_ZERO(w[1]));
	    assertTrue(IS_EQUAL(w[2],19));

	    // Test matrix rotate
	    v = Vector(1,0,0);
	    op1 = Matrix::matrixRotate(Vector(0,0,1),90);
	    assertTrue(op1.isOrthogonal());
	    assertTrue(op1*v == Vector(0,-1,0));
	    op1 = Matrix::matrixRotate(Vector(0,0,1),180);
	    assertTrue(op1*v == Vector(-1,0,0));
	    op1 = Matrix::matrixRotate(Vector(0,0,1),35);
	    op2 = Matrix::matrixRotate(Vector(0,0,1),-35);
	    assertTrue(op1*op2*v == v);
	    assertTrue(op2*op1*v == v);
	    op1 = Matrix::matrixRotate(Vector(3,4,6),360);
	    assertTrue(op1*v == v);
	    op1 = Matrix::matrixRotate(Vector(3,4,6),180);
	    op2 = Matrix::matrixRotate(Vector(3,4,6),180);
	    assertTrue((op1*op2).isOrthogonal());
	    assertTrue(op1*op2*v == v);

	    // Test matrix translate
	    v = Vector(3,4,5);
	    op1 = Matrix::matrixTranslate(Vector(2,3,4));
	    assertFalse(op1.isOrthogonal());
	    assertTrue(op1*v == Vector(5,7,9));
	    op1 = Matrix::matrixTranslate(Vector(2,3,4));
	    op2 = Matrix::matrixTranslate(Vector(-2,-3,-4));
	    assertTrue(op1*op2*v == v);
	    assertTrue(op2*op1*v == v);

	    // Test matrix scale
	    v = Vector(3,4,5);
	    op1 = Matrix::matrixScale(Vector(2,2,2));
	    assertFalse(op1.isOrthogonal());
	    assertFalse(op1.isIdentity());
	    assertTrue(op1*v == Vector(6,8,10));
	    op1 = Matrix::matrixScale(Vector(5,6,7));
	    assertFalse(op1.isOrthogonal());
	    assertFalse(op1.isIdentity());
	    assertTrue(op1*v == Vector(15,24,35));
	    op1 = Matrix::matrixScale(Vector(5,6,7));
	    op2 = Matrix::matrixScale(Vector(1.0/5.0,1.0/6.0,1.0/7.0));
	    assertFalse(op1.isOrthogonal());
	    assertFalse(op1.isIdentity());
	    assertFalse(op2.isOrthogonal());
	    assertFalse(op2.isIdentity());
	    assertTrue(op1*op2*v == v);
	    assertTrue(op2*op1*v == v);
	}
};

class binomial_test : public Test {
    public:
	void run() {
	    assertTrue(Math::binomialCoefficient(7,-3) == 0);
	    assertTrue(Math::binomialCoefficient(-7,3) == 0);
	    assertTrue(Math::binomialCoefficient(-7,-3) == 0);
	    assertTrue(Math::binomialCoefficient(7,0) == 1);

	    assertTrue(Math::binomialCoefficient(7,3) == 35);
	    assertTrue(Math::binomialCoefficient(8,3) == 56);
	    assertTrue(Math::binomialCoefficient(16,6) == 8008);
	    assertTrue(Math::binomialCoefficient(20,7) == 77520);
	    assertTrue(Math::binomialCoefficient(15,15) == 1);
	    assertTrue(Math::binomialCoefficient(15,11) == 1365);
	    assertTrue(Math::binomialCoefficient(29,27) == 406);
	    assertTrue(Math::binomialCoefficient(35,5) == 324632);
	    assertTrue(Math::binomialCoefficient(50,1) == 50);
	    assertTrue(Math::binomialCoefficient(50,2) == 1225);
	    assertTrue(Math::binomialCoefficient(59,4) == 455126);
	}
};

// See http://mathworld.wolfram.com/BernsteinPolynomial.html
class bernstein_polynomial_test : public Test {
    public:
	void run() {
	    // 1
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(0,0,0.5),1));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(0,0,1),1));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(0,0,0),1));
	    // 1 - t
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(0,1,0.2),0.8));
	    // t
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(1,1,0.2),0.2));
	    // 2(1-t)t
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(1,2,3),-12));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(1,2,0.5),0.5));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(1,2,1),0));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(1,2,0),0));
	    // t^2
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(2,2,5),25));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(2,2,1),1));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(2,2,0),0));
	    // t^3
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(3,3,5),125));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(3,3,1),1));
	    assertTrue(IS_EQUAL(Math::bernsteinPolynomial(3,3,0),0));
	}
};

class clamp_test : public Test {
    public:
	void run() {
	    assertTrue(Math::clamp(0.5) == 0.5);
	    assertTrue(Math::clamp(0.0) == 0.0);
	    assertTrue(Math::clamp(1.0) == 1.0);
	    assertTrue(Math::clamp(-0.5) == 0.0);
	    assertTrue(Math::clamp(1.5) == 1.0);
	}
};

// Test cubic root
class test_cubic_root : public Test {
    public:
	void run() {
	    assertTrue(IS_EQUAL(cbrt(125),5));
	    assertTrue(IS_EQUAL(cbrt( 64),4));
	    assertTrue(IS_EQUAL(cbrt( 27),3));
	    assertTrue(IS_EQUAL(cbrt(  8),2));
	    assertTrue(IS_EQUAL(cbrt(  1),1));
	}
};


// Returns true if the array with num elements contains val
bool contains(double* array, unsigned int num, double val) {
    for(unsigned int i = 0; i < num; i++) {
	if (IS_EQUAL(array[i],val)) {
	    return true;
	}
    }
    return false;
}

// Used /usr/bin/gp to find test polynomials
// Part of 'pari-gp' Debian package
class solve_quartic_test : public Test {

    public:
	void run() {
	    double roots[4];

	    // x^4 - 10*x^3 + 35*x^2 - 50*x + 24 = (x-1)*(x-2)*(x-3)*(x-4)
	    assertTrue(Math::solveQuartic(-10,35,-50,24,roots) == 4);
	    assertTrue(IS_EQUAL(roots[0],1));
	    assertTrue(IS_EQUAL(roots[1],2));
	    assertTrue(IS_EQUAL(roots[2],3));
	    assertTrue(IS_EQUAL(roots[3],4));
	    
	    Math::solveQuarticSingle(-10,35,-50,24,0.0,roots);
	    assertTrue(IS_EQUAL(roots[0],1));
	    Math::solveQuarticSingle(-10,35,-50,24,2.5,roots);
	    assertTrue(IS_EQUAL(roots[0],3));

	    // x^4 - 11*x^3 + 44*x^2 - 76*x + 48 = (x-2)*(x-2)*(x-3)*(x-4)
	    assertTrue(Math::solveQuartic(-11,44,-76,48,roots) == 3);
	    assertTrue(IS_EQUAL(roots[0],2));
	    assertTrue(IS_EQUAL(roots[1],3));
	    assertTrue(IS_EQUAL(roots[2],4));

	    // x^4 - 14*x^3 + 71*x^2 - 154*x + 120 = (x-5)*(x-4)*(x-3)*(x-2)
	    assertTrue(Math::solveQuartic(-14,71,-154,120,roots) == 4);
	    assertTrue(IS_EQUAL(roots[0],2));
	    assertTrue(IS_EQUAL(roots[1],3));
	    assertTrue(IS_EQUAL(roots[2],4));
	    assertTrue(IS_EQUAL(roots[3],5));

	    // x^4 - 50*x^2 + 625 = (x-5)*(x+5)*(x-5)*(x+5)
	    assertTrue(Math::solveQuartic(0,-50,0,625,roots) == 2);
	    assertTrue(IS_EQUAL(roots[0],-5));
	    assertTrue(IS_EQUAL(roots[1],5));

	    Math::solveQuarticSingle(0,-50,0,625,0.0,roots);
	    assertTrue(IS_EQUAL(roots[0],5));
	    Math::solveQuarticSingle(0,-50,0,625,-10.0,roots);
	    assertTrue(IS_EQUAL(roots[0],-5));

	    // x^4 - 100*x^3 + 3500*x^2 - 50000*x + 240000 = (x-10)*(x-20)*(x-30)*(x-40)
	    assertTrue(Math::solveQuartic(-100,3500,-50000,240000,roots) == 4);
	    assertTrue(IS_EQUAL(roots[0],10));
	    assertTrue(IS_EQUAL(roots[1],20));
	    assertTrue(IS_EQUAL(roots[2],30));
	    assertTrue(IS_EQUAL(roots[3],40));

	    //  x^4 + 10*x^2 + 24 = (x^2+4)*(x^2+6)
	    assertTrue(Math::solveQuartic(0,10,0,24,roots) == 0);

	    //  x^4 
	    assertTrue(Math::solveQuartic(0,0,0,0,roots) == 1);
	    assertTrue(IS_EQUAL(roots[0],0));
	    assertTrue(Math::solveQuarticSingle(0,0,0,0,-1,roots) != 0);
	    assertTrue(IS_EQUAL(roots[0],0));
	    assertTrue(Math::solveQuarticSingle(0,0,0,0,EPSILON,roots) == 0);

	    // x^4 - 4*x^3 + 6*x^2 - 4*x + 1 =  (x-1)(x-1)(x-1)(x-1)
	    assertTrue(Math::solveQuartic(-4,6,-4,1,roots) == 1);
	    assertTrue(IS_EQUAL(roots[0],1));
	    assertTrue(Math::solveQuarticSingle(-4,6,-4,1,0.0,roots) != 0);
	    assertTrue(IS_EQUAL(roots[0],1));
	    assertTrue(Math::solveQuarticSingle(-4,6,-4,1,2.0,roots) == 0);
	    assertTrue(Math::solveQuarticSingle(-4,6,-4,1,-1.0,roots) != 0);

	    // x^4 - 10*x^3 + 250*x - 625 = (x-5)*(x-5)*(x-5)*(x+5)
	    assertTrue(Math::solveQuartic(-10,0,250,-625,roots) == 2);
	    assertTrue(IS_EQUAL(roots[0],-5));
	    assertTrue(IS_EQUAL(roots[1],5));

	    // x^4 + 2*x^3 - 2*x - 1 = (x+1)*(x+1)*(x+1)*(x-1)
	    assertTrue(Math::solveQuartic(2,0,-2,-1,roots) == 2);
	    assertTrue(IS_EQUAL(roots[0],-1));
	    assertTrue(IS_EQUAL(roots[1],1));

	    // x^4 + 10*x^3 - 250*x - 625 = (x+5)*(x+5)*(x-5)*(x+5)
	    assertTrue(Math::solveQuartic(10,0,-250,-625,roots) == 2);
	    assertTrue(IS_EQUAL(roots[0],-5));
	    assertTrue(IS_EQUAL(roots[1],5));

	    int n = 5;
	    int num;
	    for(int A = -n; A < n; A++) {
		for(int B = -n; B < n; B++) {
		    for(int C = -n; C < n; C++) {
			for(int D = -n; D < n; D++) {
			    num = Math::solveQuartic(A,B,C,D,roots);
			    if (!check_roots(A,B,C,D,roots,num)) {
				cout << "A,B,C,D = " << A << "," << B << "," << C << "," << D  << " failed." << endl;
				cout << num << " roots found: ";
				for(int i = 0; i < num; i++) {
				    cout << roots[i] << " and ";
				}
				cout << endl;
				double a = -B;
				double b = A*C - 4*D;
				double c = 4*B*D - C*C - A*A*D;
				double cubic_roots[3];
				//double tmp1, tmp2;
				Math::solveCubic(a,b,c,cubic_roots);
				double y = cubic_roots[0];
				double R = 0.25*A*A - B + y;

				cout << "R = " << R << endl;
				cout << "R == 0: " << (IS_ZERO(R) ? "yes" : "no") << endl;

				cout << endl;
				cout << endl;
				assertTrue(false);
				//		exit(EXIT_FAILURE);
			    }
			}
		    }
		}
	    }
	}

    private:
	bool check_roots(double A, double B, double C, double D, double* roots, int num) {
	    double r;
	    for(int i = 0; i < num; i++) {
		r = roots[i];
		double val = r*r*r*r + A*r*r*r + B*r*r + C*r + D;
		if (!IS_ZERO(val)) {
		    cout << "Problem: f(" << r << ") = " << val << " != 0." << endl;
		    //    if (fabs(val) > 0.001)
		    return false;
		}
		if (i > 0 && roots[i] < roots[i-1]) {
		    cout << "Problem: wrong order of roots." << endl;
		    return false;
		}
	    }
	    return true;
	}

};

class solve_cubic_test : public Test {
    public:
	void run() {
	    double roots[3];

	    // x^3 - 2x^2 -x + 2 = (x-2)*(x-1)*(x+1)
	    assertTrue(Math::solveCubic(-2,-1,2,roots) == 3);
	    assertTrue(contains(roots,3,1));
	    assertTrue(contains(roots,3,-1));
	    assertTrue(contains(roots,3,2));

	    // x^3 - 8 = 0 
	    assertTrue(Math::solveCubic(0,0,-8,roots) == 1);
	    assertTrue(contains(roots,1,2));

	    // x^3 - 3*x^2 + 3*x - 1 = (x-1)*(x-1)*(x-1)
	    assertTrue(Math::solveCubic(-3,3,-1,roots) == 1);
	    assertTrue(contains(roots,1,1));

	    // x^3 - 4*x^2 + 5*x - 2 = (x-1)*(x-1)*(x-2)
	    assertTrue(Math::solveCubic(-4,5,-2,roots) == 2);
	    assertTrue(contains(roots,2,1));
	    assertTrue(contains(roots,2,2));

	    // x^3 + 2*x^2 - 29*x - 30 = (x+1)*(x+6)*(x-5)
	    assertTrue(Math::solveCubic(2,-29,-30,roots) == 3);
	    assertTrue(contains(roots,3,-1));
	    assertTrue(contains(roots,3,-6));
	    assertTrue(contains(roots,3,5));

	    // x^3 + 5*x^2 - 50*x = x*(x-5)*(x+10)
	    assertTrue(Math::solveCubic(5,-50,0,roots) == 3);
	    assertTrue(contains(roots,3,0));
	    assertTrue(contains(roots,3,5));
	    assertTrue(contains(roots,3,-10));

	    // x^3 + 5*x^2 - 25*x - 125 = (x+5)*(x+5)*(x-5)
	    assertTrue(Math::solveCubic(5,-25,-125,roots) == 2);
	    assertTrue(contains(roots,2,-5));
	    assertTrue(contains(roots,2,5));

	    // x^3 - 3*x - 2 = (x+1)*(x+1)*(x-2)
	    assertTrue(Math::solveCubic(0,-3,-2,roots) == 2);
	    assertTrue(contains(roots,2,-1));
	    assertTrue(contains(roots,2,2));

	    // x^3 - 3*x - 2 = (x+1)*(x+1)*(x-2)
	    assertTrue(Math::solveCubic(0,-12,-16,roots) == 2);
	    assertTrue(contains(roots,2,-2));
	    assertTrue(contains(roots,2,4));

	    // x^3 + x^2 = x*x*(x+1)
	    assertTrue(Math::solveCubic(1,0,0,roots) == 2);
	    assertTrue(contains(roots,2,0));
	    assertTrue(contains(roots,2,-1));

	    // x^3 - x^2 - x + 1= (x+1)*(x-1)*(x-1)
	    assertTrue(Math::solveCubic(-1,-1,1,roots) == 2);
	    assertTrue(contains(roots,2,1));
	    assertTrue(contains(roots,2,-1));

	    // x^3 
	    assertTrue(Math::solveCubic(0,0,0,roots) == 1);
	    assertTrue(contains(roots,1,0));

	    // x^3 - 3
	    assertTrue(Math::solveCubic(0,0,-3,roots) == 1);
	    assertTrue(contains(roots,1,cbrt(3.0)));
	    
	    /*
	    double A = 0.0;
	    double B = 0.0;
	    double C = -3.0;
	    double Q = (3.0 * B - A * A) / 9.0;
	    double R = (9.0 * A * B - 27.0 * C - 2.0 * A * A * A) / 54.0;
	    double D = Q * Q * Q + R * R;
	    double sqrtD = sqrt(D);
	    double S = cbrt(R + sqrtD);
	    double T = cbrt(R - sqrtD);
	    double r0 = S + T - A/3.0;
	    double r1 =-0.5 * (S+T) - A/3.0;
	    cout << "Q = " << Q << endl;
	    cout << "R = " << R << endl;
	    cout << "D = " << D << endl;
	    cout << "sqrtD = " << sqrtD << endl;
	    cout << "S = " << S << endl;
	    cout << "T = " << T << endl;
	    cout << "r0 = " << r0 << endl;
	    cout << "r1 = " << r1 << endl;
	    cout << "Equal = " << (IS_EQUAL(r0,r1) ? "yes" : "no") << endl;
	    */

	    int n = 5;
	    int num;
	    for(int A = -n; A < n; A++) {
		for(int B = -n; B < n; B++) {
		    for(int C = -n; C < n; C++) {
			double a = double(A);
			double b = double(B);
			double c = double(C);
			num = Math::solveCubic(a,b,c,roots);
			if (!check_cubic_roots(a,b,c,roots,num)) {
			    cout << num << " roots found: ";
			    for(int i = 0; i < num; i++) {
				cout << roots[i] << " and ";
			    }
			    cout << endl;
			    assertTrue(false);
			    //		exit(EXIT_FAILURE);
			}
		    }
		}
	    }
	}

    private:
	bool check_cubic_roots(double A, double B, double C, double* roots, int num) {
	    double r;
	    for(int i = 0; i < num; i++) {
		r = roots[i];
		double val = r*r*r + A*r*r + B*r + C;
		if (!IS_ZERO(val)) {
		    cout << "A,B,C = " << A << "," << B << "," << C << " failed." << endl;
		    cout << "Roots found: " << num << endl;
		    cout << "Cubic problem: f(" << r << ") = " << val << " != 0." << endl;
		    //if (fabs(val) > 0.001)
		    return false;
		}
	    }
	    return true;
	}

};

class solve_quadratic_test : public Test  {

    public:
	void run() {
	    double roots[2];
	    assertTrue(Math::solveQuadratic(1,0,0,roots) == 1);
	    assertTrue(IS_EQUAL(roots[0],0));

	    assertTrue(Math::solveQuadratic(1,0,-4,roots) == 2);
	    assertTrue(contains(roots,2,2));
	    assertTrue(contains(roots,2,-2));

	    assertTrue(Math::solveQuadratic(2,0,-8,roots) == 2);
	    assertTrue(contains(roots,2,2));
	    assertTrue(contains(roots,2,-2));

	    assertTrue(Math::solveQuadratic(1,5,6,roots) == 2);
	    assertTrue(contains(roots,2,-2));
	    assertTrue(contains(roots,2,-3));

	    assertTrue(Math::solveQuadratic(2,1,0,roots) == 2);
	    assertTrue(contains(roots,2,-0));
	    assertTrue(contains(roots,2,-0.5));

	    assertTrue(Math::solveQuadratic(5,5,2,roots) == 0);

	    assertTrue(Math::solveQuadratic(0,1,-2,roots) == 1);
	    assertTrue(contains(roots,1,2));

	    assertTrue(Math::solveQuadratic(0,5,10,roots) == 1);
	    assertTrue(contains(roots,1,-2));

	    assertTrue(Math::solveQuadratic(1,0,-25,roots) == 2);
	    assertTrue(contains(roots,2,5));
	    assertTrue(contains(roots,2,-5));

	    // -2*x^2 - 2 = 0
	    assertTrue(Math::solveQuadratic(-2,0,-2,roots) == 0);

	    int n = 5;
	    int num;
	    for(int A = -n; A < n; A++) {
		for(int B = -n; B < n; B++) {
		    for(int C = -n; C < n; C++) {
			if (A == 0 && B == 0)
			    continue;
			num = Math::solveQuadratic(A,B,C,roots);
			if (!check_roots(A,B,C,roots,num)) {
			    cout << num << " roots found: ";
			    for(int i = 0; i < num; i++) {
				cout << roots[i] << " and ";
			    }
			    cout << endl;
			    assertTrue(false);
			    //		exit(EXIT_FAILURE);
			}
		    }
		}
	    }
	}

    private:
	bool check_roots(double A, double B, double C, double* roots, int num) {
	    double r;
	    for(int i = 0; i < num; i++) {
		r = roots[i];
		double val = A*r*r + B*r + C;
		if (!IS_ZERO(val)) {
		    cout << "A,B,C = " << A << "," << B << "," << C << " failed." << endl;
		    cout << "Problem: f(" << r << ") = " << val << " != 0." << endl;
		    return false;
		}
	    }
	    return true;
	}

};


class perturb_vector_test : public Test  {
    public:
	void run() {
	    double max_angle = DEG2RAD(10.0);
	    for(int i = 0; i < 10000; i++) {
		Vector axis = Vector(RANDOM(-1,1),RANDOM(-1,1),RANDOM(-1,1));
		axis.normalize();
		Vector perturbed = Math::perturbVector(axis,max_angle);
		perturbed.normalize();
		double angle = acos(axis*perturbed);
		assertTrue(angle <= max_angle);
	    }
	}
};

class sinFunc : public Function<double,double> {
    public:
	double eval(const double& x) const {
	    return sin(x);
	}
};

class polyFunc: public Function<double,double> {
    public:
	double eval(const double& x) const {
	    return (x-4)*(x-6)*(x+7);
	}
};

class brents_method : public Test  {
    public:
	void run() {
	    sinFunc my_sin = sinFunc();
	    polyFunc my_poly = polyFunc();

	    double root;
	    // sin(x)
	    RootFinder rf = RootFinder(RootFinder::BRENTS_METHOD,0.001,&my_sin);
            assertTrue(rf.solve(M_PI-0.5, M_PI+0.5, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertTrue(rf.solve(M_PI-0.01, 2*M_PI-0.01, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertTrue(rf.solve(M_PI-0.5, M_PI+0.1, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertFalse(rf.solve(M_PI+0.1, M_PI+0.2, &root));
            assertFalse(rf.solve(M_PI/2.0-0.1, M_PI/2.0+0.2, &root));

	    // (x-4)*(x-6)*(x+7)
	    rf = RootFinder(RootFinder::BRENTS_METHOD,EPSILON,&my_poly);
	    assertTrue(rf.solve(3.9,4.2,&root));
	    assertTrue(IS_EQUAL(root,4.0));
	    assertTrue(IS_ZERO(my_poly(root)));
	    assertTrue(rf.solve(5.2,6.4,&root));
	    assertTrue(IS_EQUAL(root,6.0));
	    assertTrue(IS_ZERO(my_poly(root)));
	    assertTrue(rf.solve(-10,-2,&root));
	    assertTrue(IS_EQUAL(root,-7.0));
	    assertTrue(IS_ZERO(my_poly(root)));

	    // x^3 + 2*x^2 - 29*x - 30 = (x+1)*(x+6)*(x-5)
	    Polynomial poly = Polynomial(1,2,-29,-30);
	    rf = RootFinder(RootFinder::BRENTS_METHOD,EPSILON,&poly);
	    assertTrue(rf.solve(-0.9,-1.1,&root));
	    assertTrue(IS_EQUAL(root,-1.0));
	    assertTrue(IS_ZERO(poly(root)));
	    assertTrue(rf.solve(4.1,5.4,&root));
	    assertTrue(IS_EQUAL(root,5.0));
	    assertTrue(IS_ZERO(poly(root)));
	    assertTrue(rf.solve(-10,-5,&root));
	    assertTrue(IS_EQUAL(root,-6.0));
	    assertTrue(IS_ZERO(poly(root)));
	}
};

class regula_falsi : public Test  {
    public:
	void run() {
	    sinFunc my_sin = sinFunc();
	    polyFunc my_poly = polyFunc();
	    double root;

	    // sin(x)
	    RootFinder rf = RootFinder(RootFinder::REGULA_FALSI,0.001,&my_sin);
            assertTrue(rf.solve(M_PI-0.5, M_PI+0.5, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertTrue(rf.solve(M_PI-0.01, 2*M_PI-0.01, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertTrue(rf.solve(M_PI-0.5, M_PI+0.1, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertFalse(rf.solve(M_PI+0.1, M_PI+0.2, &root));
            assertFalse(rf.solve(M_PI/2.0-0.1, M_PI/2.0+0.2, &root));

	    // (x-4)*(x-6)*(x+7)
	    rf = RootFinder(RootFinder::REGULA_FALSI,EPSILON,&my_poly);
	    assertTrue(rf.solve(3.9,4.2,&root));
	    assertTrue(IS_EQUAL(root,4.0));
	    assertTrue(IS_ZERO(my_poly(root)));
	    assertTrue(rf.solve(5.2,6.4,&root));
	    assertTrue(IS_EQUAL(root,6.0));
	    assertTrue(IS_ZERO(my_poly(root)));
	    assertTrue(rf.solve(-10,-2,&root));
	    assertTrue(IS_EQUAL(root,-7.0));
	    assertTrue(IS_ZERO(my_poly(root)));

	}
};

class bisection : public Test  {
    public:
	void run() {
	    sinFunc my_sin = sinFunc();
	    polyFunc my_poly = polyFunc();
	    double root;

	    // sin(x)
	    RootFinder rf = RootFinder(RootFinder::BISECTION,0.001,&my_sin);
            assertTrue(rf.solve(M_PI-0.5, M_PI+0.5, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertTrue(rf.solve(M_PI-0.01, 2*M_PI-0.01, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertTrue(rf.solve(M_PI-0.5, M_PI+0.1, &root));
	    assertTrue(fabs(my_sin(root)) < 0.001);
            assertFalse(rf.solve(M_PI+0.1, M_PI+0.2, &root));
            assertFalse(rf.solve(M_PI/2.0-0.1, M_PI/2.0+0.2, &root));

	    // (x-4)*(x-6)*(x+7)
	    rf = RootFinder(RootFinder::BISECTION,EPSILON,&my_poly);
	    assertTrue(rf.solve(3.9,4.2,&root));
	    assertTrue(IS_EQUAL(root,4.0));
	    assertTrue(IS_ZERO(my_poly(root)));
	    assertTrue(rf.solve(5.2,6.4,&root));
	    assertTrue(IS_EQUAL(root,6.0));
	    assertTrue(IS_ZERO(my_poly(root)));
	    assertTrue(rf.solve(-10,-2,&root));
	    assertTrue(IS_EQUAL(root,-7.0));
	    assertTrue(IS_ZERO(my_poly(root)));

	}
};

class rootfinding_test: public Test  {
    public:
	void run() {
	    sinFunc my_sin = sinFunc();
	    polyFunc my_poly = polyFunc();

	    //double root;
	    double tole = EPSILON;
	    RootFinder brent = RootFinder(RootFinder::BRENTS_METHOD,tole,&my_sin);
	    RootFinder bisec = RootFinder(RootFinder::BISECTION,tole,&my_sin);
	    RootFinder false1 = RootFinder(RootFinder::REGULA_FALSI,tole,&my_sin);

	    /*
	    cout << "Brent, sin: " << brent.solve(M_PI-0.7, M_PI+0.5, &root) << endl;
	    cout << "Bisec, sin: " << bisec.solve(M_PI-0.7, M_PI+0.5, &root) << endl;
	    cout << "Regula, sin: " << false1.solve(M_PI-0.7, M_PI+0.5, &root) << endl;
	    */
	
	    RootFinder brent2 = RootFinder(RootFinder::BRENTS_METHOD,tole,&my_poly);
	    RootFinder bisec2 = RootFinder(RootFinder::BISECTION,tole,&my_poly);
	    RootFinder false2 = RootFinder(RootFinder::REGULA_FALSI,tole,&my_poly);
	
	    /*
	    cout << "Brent, poly: " << brent2.solve(M_PI, 4.26, &root) << endl;
	    cout << "Bisec, poly: " << bisec2.solve(M_PI, 4.26, &root) << endl;
	    cout << "False, poly: " << false2.solve(M_PI, 4.26, &root) << endl;
	    */

	}
};

class polynomials : public Test  {
    public:
	void run() {
            // Order
	    assertTrue(Polynomial(7,3,4,5).order() == 3);
	    assertTrue(Polynomial(0,3,4,5).order() == 2);
	    assertTrue(Polynomial(3,4,5).order() == 2);
	    assertTrue(Polynomial(4,5).order() == 1);
	    assertTrue(Polynomial(4).order() == 0);
	    
	    // Derivation
	    assertTrue(Polynomial(7,3,4,5).derivative() == Polynomial(21,6,4));
	    assertTrue(Polynomial(3,4,5).derivative() == Polynomial(6,4));
	    assertTrue(Polynomial(1,2).derivative() == Polynomial(1));
	    assertTrue(Polynomial(5.0).derivative() == Polynomial(0.0));
	    assertTrue(Polynomial(5.0).derivative().order() == 0);
	    assertTrue(Polynomial(0.0).derivative() == Polynomial(0.0));
	    assertFalse(Polynomial(1,2).derivative() == Polynomial(5));

	    // Evaluation
	    assertTrue(IS_EQUAL(Polynomial(1,2).eval(1.0),3.0));
	    assertTrue(IS_EQUAL(Polynomial(2,1).eval(1.0),3.0));
	    assertTrue(IS_EQUAL(Polynomial(2,1).eval(2.0),5.0));
	    assertTrue(IS_EQUAL(Polynomial(1,2,1).eval(1.0),4.0));
	    assertTrue(IS_EQUAL(Polynomial(1,2,1).eval(2.0),9.0));

	    // Auto-reduce
	    assertTrue(Polynomial(0,1,2) == Polynomial(1,2));
	    assertTrue(Polynomial(0,0,2,3) == Polynomial(2,3));
	    assertTrue(Polynomial(0,0,2,3,5) == Polynomial(2,3,5));
	    assertTrue(Polynomial(1,0,2,3) == Polynomial(1,0,2,3));
	    assertFalse(Polynomial(1,0,2,3) == Polynomial(0,0,2,3));
	    
	    double c1[] = { 7,6,5,4,3,2,0 };
	    double c2[] = { 7,6,5,4,3,2 };
	    assertTrue(Polynomial(c1,7) == Polynomial(c2,6));

	    // Adding and subtracting
	    assertTrue(Polynomial(1,2,3) + Polynomial(7,6,4,2) == Polynomial(7,7,6,5));
	    assertTrue(Polynomial(7,6,4,2) + Polynomial(1,2) == Polynomial(7,6,5,4));
	    assertTrue(Polynomial(7) + Polynomial(0,1,2,3) == Polynomial(1,2,10));
	    assertTrue(Polynomial(4,7) + Polynomial(-4,-7) == Polynomial(0));

	    assertTrue(Polynomial(7,6,4,2) - Polynomial(1,2) == Polynomial(7,6,3,0));
	    assertTrue(Polynomial(7) - Polynomial(0,1,2,3) == Polynomial(-1,-2,4));

	    Polynomial x = Polynomial(4,3,2,1);
	    Polynomial y = Polynomial(2,1,0,0);
	    x = x - y;
	    assertTrue(x == Polynomial(2,2,2,1));

	    // Multiplication by scalar
	    assertTrue(Polynomial(7,6,4,2) * 2.0 == Polynomial(14,12,8,4));
	    assertTrue(Polynomial(1,2,3,4) * 0.0 == Polynomial(0));
	    
	    // Multiplication by polynomial
	    //     (x+1)*(x+6)*(x-5) = x^3 + 2*x^2 - 29*x - 30 
	    //           (x+6)*(x-5) = x^2 + x - 30
	    //     (x+1)*(x+6)       = x^2 + 7*x + 6
	    assertTrue(Polynomial(1,1) * Polynomial(1,6) == Polynomial(1,7,6));
	    assertTrue(Polynomial(1,1) * Polynomial(1,6) * Polynomial(1,-5) == Polynomial(1,2,-29,-30));
	    assertTrue(Polynomial(1,6) * Polynomial(1,-5) == Polynomial(1,1,-30));
	    assertTrue(Polynomial(1,6,7) * Polynomial(1,0) == Polynomial(1,6,7,0));
	    assertTrue(Polynomial(1,6,7) * Polynomial(1,0,0) == Polynomial(1,6,7,0,0));
	    Polynomial A = Polynomial(1,1);
	    Polynomial B = Polynomial(1,6);
	    Polynomial C = Polynomial(1,-5);
	    Polynomial D = A * B * C;
	    assertTrue(A * B * C == C * B * A);
	    assertTrue(A * B * C * A * A * A * D == C * B * A * A * A * A * D);

	    // Division 
	    assertTrue(Polynomial(4,2,1) / 2.0 == Polynomial(2,1,0.5));
	    assertTrue(Polynomial(8) / 4.0 == Polynomial(2.0));

	    // timesX
	    assertTrue(Polynomial(4,2,1).timesX(1) == Polynomial(4,2,1,0));
	    assertTrue(Polynomial(4,2).timesX(2) == Polynomial(4,2,0,0));
	    assertTrue(Polynomial(4,2).timesX(0) == Polynomial(4,2));

	    // leading coefficient
	    assertTrue(IS_EQUAL(Polynomial(4,2,1).leadingCoefficient(),4));
	    assertTrue(IS_EQUAL(Polynomial(3,0,1).leadingCoefficient(),3));
	    assertTrue(IS_EQUAL(Polynomial(8,9,2,1).leadingCoefficient(),8));

	    // Copy constructor
	    Polynomial a = Polynomial(1,2,3);
	    Polynomial b = a;

	    b = a;
	    b = a * 2.0;
	    assertTrue(IS_EQUAL(a.leadingCoefficient(), 1.0));
	    assertTrue(IS_EQUAL(b.leadingCoefficient(), 2.0));

	    // Long division 
	    //     (x+1)*(x+6)*(x-5) = x^3 + 2*x^2 - 29*x - 30 
	    //           (x+6)*(x-5) = x^2 + x - 30
	    //     (x+1)*(x+6)       = x^2 + 7*x + 6
	    Polynomial rem;
	    Polynomial p = Polynomial(1,2,-29,-30);
	    assertTrue(p.division(Polynomial(1,1),rem) == Polynomial(1,1,-30));
	    assertTrue(rem == Polynomial(0));
	    assertTrue(p.division(Polynomial(1,-5),rem) == Polynomial(1,7,6));
	    assertTrue(rem == Polynomial(0));

	    // Fra http://www.sosmath.com/algebra/factor/fac01/fac01.html
	    p = Polynomial(3,-2,4,-3);
	    assertTrue(p.division(Polynomial(1,3,3),rem) == Polynomial(3,-11));
	    assertTrue(rem == Polynomial(28,30));

	    p = Polynomial(1,0,0,-1);
	    assertTrue(p.division(Polynomial(1,2),rem) == Polynomial(1,-2,4));
	    assertTrue(rem == Polynomial(-9));

	    p = Polynomial(1,-5,3,-15);
	    assertTrue(p.division(Polynomial(1,0,3),rem) == Polynomial(1,-5));
	    assertTrue(rem == Polynomial(0));

	    p = Polynomial(1,0,0,0,4);
	    assertTrue(p.division(Polynomial(1,0,-5),rem) == Polynomial(1,0,5));
	    assertTrue(rem == Polynomial(29));

	    p = Polynomial(1,-3,5,-3);
	    assertTrue(p.division(Polynomial(1,-1),rem) == Polynomial(1,-2,3));
	    assertTrue(rem == Polynomial(0));
	}
};

class sturm_sequence_test : public Test  {
    public:
	void run() {
	    // (x-2)*(x-4)*(x-1)*(x-1) = x^4 - 8x^3 + 21x^2 - 22x + 8
	    SturmSequence seq = SturmSequence(Polynomial(1,-8,21,-22,8));
	    assertTrue(seq.rootCount(0,3) == 2);
	    assertTrue(seq.rootCount(-1,0.5) == 0);
	    assertTrue(seq.rootCount(3,5) == 1);
	    assertTrue(seq.rootCount(1.5,3) == 1);
	    assertTrue(seq.rootCount(0.5,1.5) == 1);

	    // (1+x)(x-2) = x^2 - x - 2
	    seq = SturmSequence(Polynomial(1,-1,-2));
	    assertTrue(seq.rootCount(-10,-2) == 0);
	    assertTrue(seq.rootCount(-2,0) == 1);
	    assertTrue(seq.rootCount(0,1) == 0);
	    assertTrue(seq.rootCount(1,3) == 1);
	    assertTrue(seq.rootCount(3,20) == 0);
	    assertTrue(seq.rootCount(-2,3) == 2);

	    // (1+x)(x-2)(x-2) = x^3 - 3x^2 + 4
	    seq = SturmSequence(Polynomial(1,-3,0,4));
	    assertTrue(seq.rootCount(-10,-2) == 0);
	    assertTrue(seq.rootCount(-2,0) == 1);
	    assertTrue(seq.rootCount(0,1) == 0);
	    assertTrue(seq.rootCount(1,3) == 1);
	    assertTrue(seq.rootCount(3,20) == 0);
	    assertTrue(seq.rootCount(-2,3) == 2);

	    // (1-x)
	    seq = SturmSequence(Polynomial(-1,1));
	    assertTrue(seq.rootCount(-2,0) == 0);
	    assertTrue(seq.rootCount(0,2) == 1);
	    assertTrue(seq.rootCount(2,5) == 0);
	    

	//    double c[] =  {1,-3,-10,34,0,-40};
	    double c[] =  {-40,0,34,-10,-3,1};
	    seq = SturmSequence(Polynomial(c,6));
	    assertTrue(seq.rootCount(1,3) == 1);
	    
	}
};

class quaternion_test : public Test  {
    public:
	void run() {
	    p = Quaternion(1,2,3,4);
	    q = Quaternion(2,3,4,5);
	    assertTrue(p + p == Quaternion(2,4,6,8));
	    assertTrue(q + q == Quaternion(4,6,8,10));
	    assertTrue(p + q == Quaternion(3,5,7,9));
	    assertTrue(p.conjugate() == Quaternion(1,-2,-3,-4));
	    assertTrue(q.conjugate() == Quaternion(2,-3,-4,-5));
	    assertEqualF((p*q).norm(), p.norm() * q.norm());
	    assertTrue(p * 2 == Quaternion(2,4,6,8));
	    assertTrue(q * 3 == Quaternion(6,9,12,15));

	    // Rotations
	    q = Quaternion::rotation(Vector(0,0,1),90.0);
	    assertEqualF(q.norm(), 1.0);
	    assertTrue(q.rotate(Vector(1,0,0)) == Vector(0,1,0));
	    assertTrue(q.rotate(Vector(0,1,0)) == Vector(-1,0,0));
	    assertTrue(q.rotate(Vector(-1,0,0)) == Vector(0,-1,0));
	    assertTrue(q.rotate(Vector(0,-1,0)) == Vector(1,0,0));

	    q = Quaternion::rotation(Vector(0,1,0),90.0);
	    assertEqualF(q.norm(), 1.0);
	    assertTrue(q.rotate(Vector(1,0,0)) == Vector(0,0,-1));
	    assertTrue(q.rotate(Vector(0,0,-1)) == Vector(-1,0,0));

	    // toMatrix
	    q = Quaternion::rotation(Vector(0,0,1),90.0);
	    m = q.toMatrix();
	    assertEqualV(m * Vector(1,0,0), Vector(0,1,0));
	    assertEqualV(m * Vector(0,1,0), Vector(-1,0,0));
	    assertEqualV(m * Vector(-1,0,0), Vector(0,-1,0));
	    assertEqualV(m * Vector(0,-1,0), Vector(1,0,0));

	    q = Quaternion::rotation(Vector(0,1,0),90.0);
	    m = q.toMatrix();
	    assertEqualV(m * Vector(1,0,0), Vector(0,0,-1));
	    assertEqualV(m * Vector(0,0,-1),  Vector(-1,0,0));
	}

    private:
	Matrix m;
	Quaternion q;
	Quaternion p;
};

class constants_test : public Test {

    public:
	void run() {
	    // Constants
	    assertEqualF(M_SQRT3*M_SQRT3, 3);
	    assertEqualF(M_SQRT3, sqrt(double(3)));
	    assertEqualF(FRAC_1_6, 1.0 / 6.0);
	    assertEqualF(FRAC_1_3, 1.0 / 3.0);
	    assertEqualF(FRAC_1_9, 1.0 / 9.0);
	    assertEqualF(FRAC_2_3, 2.0 / 3.0);
	    assertEqualF(FRAC_1_27, 1.0 / 27.0);
	    assertEqualF(FRAC_1_54, 1.0 / 54.0);

	    // SAME_SIGN
	    assertTrue(SAME_SIGN(0,1));
	    assertTrue(SAME_SIGN(1,2));
	    assertTrue(SAME_SIGN(1,0));
	    assertTrue(SAME_SIGN(-1,-2));
	    assertFalse(SAME_SIGN(-1,0));
	    assertFalse(SAME_SIGN(-1,2));
	}
};

class poisson_disc_test : public Test {

    public:
	void run() {
	    Vector2* r = new Vector2[100];
	    int req_num = 300;
	    int num = PoissonDiscDistribution::createSet(10,10,0.5,req_num,r);
	    cout << "Darts: " << num << endl;
	    assertTrue(num <= req_num);
	    assertTrue(num > 10); 
	    for(int i = 0; i < num; i++) {
		assertTrue(r[i][0] >= 0);
		assertTrue(r[i][1] >= 0);
		assertTrue(r[i][0] <= 10);
		assertTrue(r[i][1] <= 10);
		for(int j = 0; j < num; j++) {
		    if (j != i) {
			assertTrue((r[i]-r[j]).norm() >= 2*0.5*2*0.5);
		    }
		}
	    }
	}
};

class interval_test : public Test {

    public:
	void run() {
	    // Test == and !=
	    Interval i1 = Interval(2,4);
	    Interval i2 = Interval(6,8);
	    Interval i3 = Interval(3,7);
	    assertTrue(i1 == Interval(2,4));
	    assertFalse(i1 == i2);
	    assertTrue(i1 != i3);

	    // Test contains()
	    assertTrue(i1.contains(3));
	    assertFalse(i1.contains(1));
	    assertFalse(i1.contains(5));
	    assertTrue(i3.contains(4));
	    assertTrue(i3.contains(6));
	    assertFalse(i3.contains(1));
	    assertFalse(i3.contains(8));

	    // Test length()
	    assertEqualF(i1.length(), 2);
	    assertEqualF(i2.length(), 2);
	    assertEqualF(i3.length(), 4);

	    // Test subtract()
	    i1.subtract(i2);
	    assertTrue(i1 == Interval(2,4));   // Case 2
	    i2.subtract(i1); 
	    assertTrue(i2 == Interval(6,8));   // Case 3
	    i1.subtract(3,7);
	    assertTrue(i1 == Interval(2,3));
	    i1 = Interval(2,4);
	    i1.subtract(i3);
	    assertTrue(i1 == Interval(2,3)); // Case 4
	    i2.subtract(i3);
	    assertTrue(i2 == Interval(7,8)); // Case 5
	    i1 = Interval(1,10);
	    i1.subtract(Interval(5,7));      // Case 6
	    assertFalse(i1.contains(0.0));
	    assertTrue(i1.contains(4.5));
	    assertTrue(i1.contains(9));
	    assertTrue(i1.contains(7.5));
	    assertFalse(i1.contains(5.5));
	    assertFalse(i1.contains(6.5));
	    assertEqualF(i1.length(), 4+3);
	    i1.subtract(Interval(4,8));     // Case 4 & 5 combined
	    assertEqualF(i1.length(), 3+2);
	    assertTrue(i1.contains(3.5));
	    assertFalse(i1.contains(4.5));
	    assertTrue(i1.contains(8.5));
	    assertFalse(i1.contains(7.5));
            i1.subtract(Interval(6,20));    // Case 1
	    assertTrue(i1 == Interval(1,4));
	    i1.subtract(Interval(-10,10));  // Case 1
	    assertTrue(i1.isEmpty());

	    // Test random()
	    i1 = Interval(0,100);
	    i1.subtract(20,40);
	    i1.subtract(50,70);
	    i1.subtract(80,90);
	    assertEqualF(i1.length(), 100 - 20 - 20 - 10);
	    for(int i = 0; i < 10000; i++) {
		double d = i1.random();
		assertTrue(i1.contains(d));
	    }
	}
};

int main(int argc, char *argv[]) {

    TestSuite suite;

    suite.add("Constants",new constants_test());
    suite.add("Vector",new vector_test());
    suite.add("Vector2",new vector2_test());
    suite.add("Matrix",new matrix_test());
    suite.add("Matrix3",new matrix3_test());
    suite.add("Binomial",new binomial_test());
    suite.add("Bernstein",new bernstein_polynomial_test());
    suite.add("Clamp",new clamp_test());
    suite.add("Solve quadratic",new solve_quadratic_test());
    suite.add("Solve cubic",new solve_cubic_test());
    suite.add("Cubic root",new test_cubic_root());
    suite.add("Perturb vector",new perturb_vector_test());
    suite.add("Solve quartic",new solve_quartic_test());
    suite.add("Brent's method",new brents_method());
    suite.add("Bisection",new bisection());
    suite.add("Regula falsi",new regula_falsi());
    suite.add("Polynomials",new polynomials());
    suite.add("Sturm sequence",new sturm_sequence_test());
    suite.add("Quaternion",new quaternion_test());
    suite.add("RootFinding",new rootfinding_test());
    suite.add("Poisson Disc",new poisson_disc_test());
    suite.add("Interval",new interval_test());
    suite.run();
    suite.printStatus();
    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}



