
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <iostream>

#include "math/vector.h"
#include "math/vector2.h"
#include "math/matrix.h"
#include "math/functions.h"
#include "math/rootfinder.h"
#include "math/polynomial.h"
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

	    v = v / 2;
	    assertTrue(v == Vector(1,2,3));

	    v.scale(10);
	    assertTrue(v == Vector(10,20,30));

	    v.scale(-1);
	    assertTrue(v == Vector(-10,-20,-30));

	    assertTrue(2 * v == v * 2);

	    Vector w = Vector(100,200,300);
	    assertTrue(w * v == v * w);

	    assertTrue(Vector::xProduct(v,w) == Vector::xProduct(w,v));

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

class matrix_test : public Test {
    public:
	void run() {
	    /* Test inverse() */
	    Matrix id,res;
	    Matrix op1 = Matrix::matrixRotate(Vector(10,30,19),12);
	    res = op1*id*op1.inverse();
	    assertTrue(res.isIdentity());

	    Matrix op2 = Matrix::matrixTranslate(Vector(401,221,39));
	    res = op2*id*op2.inverse();
	    assertTrue(res.isIdentity());

	    Matrix op3 = op1*op2;
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
	    assertTrue(op1 != op2);
	    op2 = op2.inverse();
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
	    assertTrue(op1*op2*v == v);

	    // Test matrix translate
	    v = Vector(3,4,5);
	    op1 = Matrix::matrixTranslate(Vector(2,3,4));
	    assertTrue(op1*v == Vector(5,7,9));
	    op1 = Matrix::matrixTranslate(Vector(2,3,4));
	    op2 = Matrix::matrixTranslate(Vector(-2,-3,-4));
	    assertTrue(op1*op2*v == v);
	    assertTrue(op2*op1*v == v);

	    // Test matrix scale
	    v = Vector(3,4,5);
	    op1 = Matrix::matrixScale(Vector(2,2,2));
	    assertTrue(op1*v == Vector(6,8,10));
	    op1 = Matrix::matrixScale(Vector(5,6,7));
	    assertTrue(op1*v == Vector(15,24,35));
	    op1 = Matrix::matrixScale(Vector(5,6,7));
	    op2 = Matrix::matrixScale(Vector(1.0/5.0,1.0/6.0,1.0/7.0));
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
    }
    return true;
}

// Used /usr/bin/gp to find test polynomials
// Part of 'pari-gp' Debian package
class solve_quartic_test : public Test {

    public:
	void run() {
	    double roots[4];

	    // x^4 - 10*x^3 + 35*x^2 - 50*x + 24 = (x-1)*(x-2)*(x-3)*(x-4)
	    assertTrue(Math::solveQuartic(-10,35,-50,24,roots) == 4);
	    assertTrue(contains(roots,4,1));
	    assertTrue(contains(roots,4,2));
	    assertTrue(contains(roots,4,3));
	    assertTrue(contains(roots,4,4));

	    // x^4 - 11*x^3 + 44*x^2 - 76*x + 48 = (x-2)*(x-2)*(x-3)*(x-4)
	    assertTrue(Math::solveQuartic(-11,44,-76,48,roots) == 3);
	    assertTrue(contains(roots,3,2));
	    assertTrue(contains(roots,3,3));
	    assertTrue(contains(roots,3,4));

	    // x^4 - 14*x^3 + 71*x^2 - 154*x + 120 = (x-5)*(x-4)*(x-3)*(x-2)
	    assertTrue(Math::solveQuartic(-14,71,-154,120,roots) == 4);
	    assertTrue(contains(roots,4,2));
	    assertTrue(contains(roots,4,3));
	    assertTrue(contains(roots,4,4));
	    assertTrue(contains(roots,4,5));

	    // x^4 - 50*x^2 + 625 = (x-5)*(x+5)*(x-5)*(x+5)
	    assertTrue(Math::solveQuartic(0,-50,0,625,roots) == 2);
	    assertTrue(contains(roots,2,-5));
	    assertTrue(contains(roots,2,5));

	    // x^4 - 100*x^3 + 3500*x^2 - 50000*x + 240000 = (x-10)*(x-20)*(x-30)*(x-40)
	    assertTrue(Math::solveQuartic(-100,3500,-50000,240000,roots) == 4);
	    assertTrue(contains(roots,4,10));
	    assertTrue(contains(roots,4,20));
	    assertTrue(contains(roots,4,30));
	    assertTrue(contains(roots,4,40));

	    //  x^4 + 10*x^2 + 24 = (x^2+4)*(x^2+6)
	    assertTrue(Math::solveQuartic(0,10,0,24,roots) == 0);

	    //  x^4 
	    assertTrue(Math::solveQuartic(0,0,0,0,roots) == 1);
	    assertTrue(contains(roots,1,0));

	    // x^4 - 4*x^3 + 6*x^2 - 4*x + 1 =  (x-1)(x-1)(x-1)(x-1)
	    assertTrue(Math::solveQuartic(-4,6,-4,1,roots) == 1);
	    assertTrue(contains(roots,1,1));

	    // x^4 - 10*x^3 + 250*x - 625 = (x-5)*(x-5)*(x-5)*(x+5)
	    assertTrue(Math::solveQuartic(-10,0,250,-625,roots) == 2);
	    assertTrue(contains(roots,2,5));
	    assertTrue(contains(roots,2,-5));

	    // x^4 + 2*x^3 - 2*x - 1 = (x+1)*(x+1)*(x+1)*(x-1)
	    assertTrue(Math::solveQuartic(2,0,-2,-1,roots) == 2);
	    assertTrue(contains(roots,2,1));
	    assertTrue(contains(roots,2,-1));

	    // x^4 + 10*x^3 - 250*x - 625 = (x+5)*(x+5)*(x-5)*(x+5)
	    assertTrue(Math::solveQuartic(10,0,-250,-625,roots) == 2);
	    assertTrue(contains(roots,2,5));
	    assertTrue(contains(roots,2,-5));

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
				assertTrue(false);
		//		exit(EXIT_FAILURE);
			    }
			}
		    }
		}
	    }
	}
};

bool check_cubic_roots(double A, double B, double C, double* roots, int num) {
    double r;
    for(int i = 0; i < num; i++) {
	r = roots[i];
	double val = r*r*r + A*r*r + B*r + C;
	if (!IS_ZERO(val)) {
	    cout << "A,B,C = " << A << "," << B << "," << C << " failed." << endl;
	    cout << "Problem: f(" << r << ") = " << val << " != 0." << endl;
	    //if (fabs(val) > 0.001)
	    return false;
	}
    }
    return true;
}

class solve_cubic_test : public Test {
    public:
	void run() {
	    double roots[3];

	    // x^3 - 2x^2 -x + 2 = (x-2)*(x-1)*(x+1)
	    assertTrue(Math::solveCubic(-2,-1,2,roots) == 3);
	    assertTrue(contains(roots,3,1));
	    assertTrue(contains(roots,3,-1));
	    assertTrue(contains(roots,3,2));

	    // x^3 - 8 = 0 = 
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

	    int n = 5;
	    int num;
	    for(int A = -n; A < n; A++) {
		for(int B = -n; B < n; B++) {
		    for(int C = -n; C < n; C++) {
			    num = Math::solveCubic(A,B,C,roots);
			    if (!check_cubic_roots(A,B,C,roots,num)) {
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

double my_sin(double x) {
    return sin(x);
}

double my_poly(double x) {
    return (x-4)*(x-6)*(x+7);
}

class brents_method : public Test  {
    public:
	void run() {
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

	}
};

class bisection : public Test  {
    public:
	void run() {
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

class polynomials : public Test  {
    public:
	void run() {
	    // Derivation
	    assertTrue(Polynomial(7,3,4,5).derivative() == Polynomial(21,6,4));
	    assertTrue(Polynomial(3,4,5).derivative() == Polynomial(6,4));
	    assertTrue(Polynomial(1,2).derivative() == Polynomial(1));
	    assertFalse(Polynomial(1,2).derivative() == Polynomial(5));

	    // Evaluation
	    assertTrue(IS_EQUAL(Polynomial(1,2).eval(1.0),3.0));
	    assertTrue(IS_EQUAL(Polynomial(2,1).eval(1.0),3.0));
	    assertTrue(IS_EQUAL(Polynomial(2,1).eval(2.0),5.0));
	    assertTrue(IS_EQUAL(Polynomial(1,2,1).eval(1.0),4.0));
	    assertTrue(IS_EQUAL(Polynomial(1,2,1).eval(2.0),9.0));
	}
};

int main(int argc, char *argv[]) {

    TestSuite suite;

    suite.add("Vector",new vector_test());
    suite.add("Vector2",new vector2_test());
    suite.add("Matrix",new matrix_test());
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
    suite.add("Polynomials",new polynomials());
    suite.run();
    suite.printStatus();
    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}

