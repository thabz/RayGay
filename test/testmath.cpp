
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <iostream>
#include <cassert>

#include "math/vector.h"
#include "math/vector2.h"
#include "math/matrix.h"
#include "math/functions.h"

using namespace std;

void vector_test() {

    cout << "Size of short: " << sizeof(short) << endl;
    cout << "Size of int: " << sizeof(int) << endl;
    cout << "Size of long: " << sizeof(long) << endl;
    cout << "Size of float: " << sizeof(float) << endl;
    cout << "Size of double: " << sizeof(double) << endl;
    cout << "Size of long double: " << sizeof(long double) << endl;
    cout << "Size of Vector: " << sizeof(Vector) << endl;
    
    Vector v = Vector(1,1,1);
    assert(IS_EQUAL(v.norm(),3));
    assert(IS_EQUAL(v.norm(),3.0));
    assert(IS_EQUAL(v.length(),sqrt(3.0)));

    v = v - v;
    assert(IS_ZERO(v.norm()));
    assert(IS_ZERO(v.length()));

    v = v + v;
    assert(IS_ZERO(v.norm()));
    assert(IS_ZERO(v.length()));

    v = v + Vector(1,2,3);
    v.normalize();
    assert(IS_EQUAL(v.norm(),1));
    assert(IS_EQUAL(v.length(),1));

    v = Vector(1,2,3);
    v = 2 * v;
    assert(v == Vector(2,4,6));

    v = v / 2;
    assert(v == Vector(1,2,3));

    v.scale(10);
    assert(v == Vector(10,20,30));

    v.scale(-1);
    assert(v == Vector(-10,-20,-30));

    assert(2 * v == v * 2);

    Vector w = Vector(100,200,300);
    assert(w * v == v * w);

    assert(Vector::xProduct(v,w) == Vector::xProduct(w,v));

    // Test polar coordinates
    v = Vector(1,0,0);
    w = v.toPolar();
    assert(IS_EQUAL(w[0],1));
    assert(!IS_EQUAL(w[1],0));
    assert(IS_EQUAL(w[2],0));

    v = Vector(0,0,1);
    w = v.toPolar();
    assert(IS_EQUAL(w[0],1));
    assert(IS_EQUAL(w[1],0));
    assert(IS_EQUAL(w[2],0));

    v = Vector(1,1,1);
    w = v.toPolar();
    assert(IS_EQUAL(w[0],sqrtf(3)));
    assert(!IS_EQUAL(w[1],0));
    assert(!IS_EQUAL(w[2],0));

    v = Vector(0,1,1);
    w = v.toPolar();
    assert(IS_EQUAL(w[0],sqrtf(2)));

    assert(Vector(10,10,10).toPolar().toRectangular() == Vector(10,10,10));
    assert(Vector(10,10,-10).toPolar().toRectangular() == Vector(10,10,-10));
    assert(Vector(10,-10,10).toPolar().toRectangular() == Vector(10,-10,10));
    assert(Vector(10,-10,-10).toPolar().toRectangular() == Vector(10,-10,-10));
    assert(Vector(-10,10,10).toPolar().toRectangular() == Vector(-10,10,10));
    assert(Vector(-10,10,-10).toPolar().toRectangular() == Vector(-10,10,-10));
    assert(Vector(-10,-10,10).toPolar().toRectangular() == Vector(-10,-10,10));
    assert(Vector(-10,-10,-10).toPolar().toRectangular() == Vector(-10,-10,-10));
    assert(Vector(0,10,10).toPolar().toRectangular() == Vector(0,10,10));

    // Test += operator
    v = Vector(1,2,3);
    v += Vector(10,20,30);
    assert(v == Vector(11,22,33));

    // Test -= operator
    v = Vector(1,2,3);
    v -= Vector(10,20,30);
    assert(v == Vector(-9,-18,-27));

    // Test *= operator
    v = Vector(1,2,3);
    v *= 2.0;
    assert(v == Vector(2,4,6));

    // Test norm
    v = Vector(10,11,12);
    w = Vector(20,31,42);
    assert(IS_EQUAL((v-w).norm(),(w-v).norm()));

    // Test scalar
    v = Vector(10,10,10);
    v.normalize();
    assert(IS_EQUAL(v*v,1));

    v = Vector(0,0,1);
    assert(IS_EQUAL(v*v,1));

    // Test area()
    assert(0.5 == Vector::area(Vector(1,1,1), Vector(2,1,1), Vector(1,2,1)));
    assert(1.0 == Vector::area(Vector(0,0,0), Vector(0,2,0), Vector(1,0,0)));
    assert(2.0 == Vector::area(Vector(0,0,0), Vector(0,2,0), Vector(2,0,0)));
}

void vector2_test() {
    Vector2 v = Vector2(1.0,2.0);
    assert(v[0] == 1);
    assert(v[1] == 2);
    v =  v * 2.0;
    Vector2 w = Vector2(2.0,4.0);
    assert(v == w);

    v = Vector2(1.0,2.0);
    w = Vector2(10.0,20.0);
    assert(w + v == Vector2(11,22));
    assert(w -v == Vector2(9,18));
    assert(w / 2.0 == Vector2(5,10));
}

void matrix_test() {
    /* Test inverse() */
    Matrix id,res;
    Matrix op1 = Matrix::matrixRotate(Vector(10,30,19),12);
    res = op1*id*op1.inverse();
    assert(res.isIdentity());

    Matrix op2 = Matrix::matrixTranslate(Vector(401,221,39));
    res = op2*id*op2.inverse();
    assert(res.isIdentity());

    Matrix op3 = op1*op2;
    res = op3*id*op3.inverse();
    assert(res.isIdentity());

    // Test == and !=
    op1.identity();
    op2.identity();
    assert(op1 == op2);
    op1 = Matrix::matrixTranslate(Vector(401,221,39));
    op2 = Matrix::matrixTranslate(Vector(-401,-221,-39));
    assert(op1 != op2);
    op2 = op2.inverse();
    assert(op1 == op2);

    op1 = Matrix::matrixRotate(Vector(401,221,39),40);
    op2 = Matrix::matrixRotate(Vector(401,221,39),-40);
    assert(op1 != op2);
    op2 = op2.inverse();
    assert(op1 == op2);

    op2 = Matrix::matrixRotate(Vector(401,221,39),-20);
    op2 = op2 * Matrix::matrixRotate(Vector(401,221,39),-20);
    assert(op1 != op2);
    op2 = op2.inverse();
    assert(op1 == op2);

    // Test matrixOrient(Vector,Vector)

    Vector v = Vector(1,1,0);
    op1 = Matrix::matrixOrient(v,Vector(0,1,0));
    op1 = op1.inverse();
    v = op1 * v;
    assert(IS_ZERO(v[0]));
    assert(IS_ZERO(v[1]));

    /// Test matrixOrient(Vector)
    v = Vector(20,391,29);
    op1 = Matrix::matrixOrient(v);
    Vector w = op1 * v;
    assert(IS_ZERO(w[0]));
    assert(IS_ZERO(w[1]));
    
    v = Vector(0,13,0);
    op1 = Matrix::matrixOrient(v);
    w = op1 * v;
    assert(IS_ZERO(w[0]));
    assert(IS_ZERO(w[1]));
    assert(IS_EQUAL(w[2],13));
    
    v = Vector(0,4,3);
    op1 = Matrix::matrixOrient(v);
    w = op1 * v;
    assert(IS_ZERO(w[0]));
    assert(IS_ZERO(w[1]));
    assert(IS_EQUAL(w[2],5));
    
    v = Vector(0,0,71);
    op1 = Matrix::matrixOrient(v);
    w = op1 * v;
    assert(IS_ZERO(w[0]));
    assert(IS_ZERO(w[1]));
    assert(IS_EQUAL(w[2],71));

    v = Vector(19,0,0);
    op1 = Matrix::matrixOrient(v);
    w = op1 * v;
    assert(IS_ZERO(w[0]));
    assert(IS_ZERO(w[1]));
    assert(IS_EQUAL(w[2],19));
}

void binomial_test() {
    assert(Math::binomialCoefficient(7,-3) == 0);
    assert(Math::binomialCoefficient(-7,3) == 0);
    assert(Math::binomialCoefficient(-7,-3) == 0);
    assert(Math::binomialCoefficient(7,0) == 1);

    assert(Math::binomialCoefficient(7,3) == 35);
    assert(Math::binomialCoefficient(8,3) == 56);
    assert(Math::binomialCoefficient(16,6) == 8008);
    assert(Math::binomialCoefficient(20,7) == 77520);
    assert(Math::binomialCoefficient(15,15) == 1);
    assert(Math::binomialCoefficient(15,11) == 1365);
}

// See http://mathworld.wolfram.com/BernsteinPolynomial.html
void bernstein_polynomial_test() {
    // 1
    assert(IS_EQUAL(Math::bernsteinPolynomial(0,0,0.5),1));
    assert(IS_EQUAL(Math::bernsteinPolynomial(0,0,1),1));
    assert(IS_EQUAL(Math::bernsteinPolynomial(0,0,0),1));
    // 1 - t
    assert(IS_EQUAL(Math::bernsteinPolynomial(0,1,0.2),0.8));
    // t
    assert(IS_EQUAL(Math::bernsteinPolynomial(1,1,0.2),0.2));
    // 2(1-t)t
    assert(IS_EQUAL(Math::bernsteinPolynomial(1,2,3),-12));
    assert(IS_EQUAL(Math::bernsteinPolynomial(1,2,0.5),0.5));
    assert(IS_EQUAL(Math::bernsteinPolynomial(1,2,1),0));
    assert(IS_EQUAL(Math::bernsteinPolynomial(1,2,0),0));
    // t^2
    assert(IS_EQUAL(Math::bernsteinPolynomial(2,2,5),25));
    assert(IS_EQUAL(Math::bernsteinPolynomial(2,2,1),1));
    assert(IS_EQUAL(Math::bernsteinPolynomial(2,2,0),0));
    // t^3
    assert(IS_EQUAL(Math::bernsteinPolynomial(3,3,5),125));
    assert(IS_EQUAL(Math::bernsteinPolynomial(3,3,1),1));
    assert(IS_EQUAL(Math::bernsteinPolynomial(3,3,0),0));
}

void clamp_test() {
    assert(Math::clamp(0.5) == 0.5);
    assert(Math::clamp(0.0) == 0.0);
    assert(Math::clamp(1.0) == 1.0);
    assert(Math::clamp(-0.5) == 0.0);
    assert(Math::clamp(1.5) == 1.0);
}

// Test cubic root
void test_cubic_root() {
    assert(IS_EQUAL(cbrt(125),5));
    assert(IS_EQUAL(cbrt( 64),4));
    assert(IS_EQUAL(cbrt( 27),3));
    assert(IS_EQUAL(cbrt(  8),2));
    assert(IS_EQUAL(cbrt(  1),1));
}

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
	if (!IS_EQUAL(val,double(0)))
	//if (fabs(val) > 0.001)
	    return false;
    }
    return true;
}

// Used /usr/bin/gp to find test polynomials
// Part of 'pari-gp' Debian package
void solve_quartic_test() {
    double roots[4];

    // x^4 - 10*x^3 + 35*x^2 - 50*x + 24 = (x-1)*(x-2)*(x-3)*(x-4)
    assert(Math::solveQuartic(-10,35,-50,24,roots) == 4);
    assert(contains(roots,4,1));
    assert(contains(roots,4,2));
    assert(contains(roots,4,3));
    assert(contains(roots,4,4));

    // x^4 - 11*x^3 + 44*x^2 - 76*x + 48 = (x-2)*(x-2)*(x-3)*(x-4)
    assert(Math::solveQuartic(-11,44,-76,48,roots) == 3);
    assert(contains(roots,3,2));
    assert(contains(roots,3,3));
    assert(contains(roots,3,4));

    // x^4 - 14*x^3 + 71*x^2 - 154*x + 120 = (x-5)*(x-4)*(x-3)*(x-2)
    assert(Math::solveQuartic(-14,71,-154,120,roots) == 4);
    assert(contains(roots,4,2));
    assert(contains(roots,4,3));
    assert(contains(roots,4,4));
    assert(contains(roots,4,5));
    
    // x^4 - 50*x^2 + 625 = (x-5)*(x+5)*(x-5)*(x+5)
    assert(Math::solveQuartic(0,-50,0,625,roots) == 2);
    assert(contains(roots,2,-5));
    assert(contains(roots,2,5));

    // x^4 - 100*x^3 + 3500*x^2 - 50000*x + 240000 = (x-10)*(x-20)*(x-30)*(x-40)
    assert(Math::solveQuartic(-100,3500,-50000,240000,roots) == 4);
    assert(contains(roots,4,10));
    assert(contains(roots,4,20));
    assert(contains(roots,4,30));
    assert(contains(roots,4,40));

    //  x^4 + 10*x^2 + 24 = (x^2+4)*(x^2+6)
    assert(Math::solveQuartic(0,10,0,24,roots) == 0);

    //  x^4 
    assert(Math::solveQuartic(0,0,0,0,roots) == 1);
    assert(contains(roots,1,0));

    // x^4 - 4*x^3 + 6*x^2 - 4*x + 1 =  (x-1)(x-1)(x-1)(x-1)
    assert(Math::solveQuartic(-4,6,-4,1,roots) == 1);
    assert(contains(roots,1,1));

    // x^4 - 10*x^3 + 250*x - 625 = (x-5)*(x-5)*(x-5)*(x+5)
    assert(Math::solveQuartic(-10,0,250,-625,roots) == 2);
    assert(contains(roots,2,5));
    assert(contains(roots,2,-5));

    // x^4 + 2*x^3 - 2*x - 1 = (x+1)*(x+1)*(x+1)*(x-1)
    assert(Math::solveQuartic(2,0,-2,-1,roots) == 2);
    assert(contains(roots,2,1));
    assert(contains(roots,2,-1));

    // x^4 + 10*x^3 - 250*x - 625 = (x+5)*(x+5)*(x-5)*(x+5)
    assert(Math::solveQuartic(10,0,-250,-625,roots) == 2);
    assert(contains(roots,2,5));
    assert(contains(roots,2,-5));

    int n = 10;
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
			exit(EXIT_FAILURE);
		    }
		}
	    }
	}
    }
}


void solve_cubic_test() {
    double roots[3];

    // x^3 - 2x^2 -x + 2 = (x-2)*(x-1)*(x+1)
    assert(Math::solveCubic(-2,-1,2,roots) == 3);
    assert(contains(roots,3,1));
    assert(contains(roots,3,-1));
    assert(contains(roots,3,2));

    // x^3 - 8 = 0 = 
    assert(Math::solveCubic(0,0,-8,roots) == 1);
    assert(contains(roots,1,2));

    // x^3 - 3*x^2 + 3*x - 1 = (x-1)*(x-1)*(x-1)
    assert(Math::solveCubic(-3,3,-1,roots) == 1);
    assert(contains(roots,1,1));

    // x^3 - 4*x^2 + 5*x - 2 = (x-1)*(x-1)*(x-2)
    assert(Math::solveCubic(-4,5,-2,roots) == 2);
    assert(contains(roots,2,1));
    assert(contains(roots,2,2));

    // x^3 + 2*x^2 - 29*x - 30 = (x+1)*(x+6)*(x-5)
    assert(Math::solveCubic(2,-29,-30,roots) == 3);
    assert(contains(roots,3,-1));
    assert(contains(roots,3,-6));
    assert(contains(roots,3,5));

    // x^3 + 5*x^2 - 50*x = x*(x-5)*(x+10)
    assert(Math::solveCubic(5,-50,0,roots) == 3);
    assert(contains(roots,3,0));
    assert(contains(roots,3,5));
    assert(contains(roots,3,-10));

    // x^3 + 5*x^2 - 25*x - 125 = (x+5)*(x+5)*(x-5)
    assert(Math::solveCubic(5,-25,-125,roots) == 2);
    assert(contains(roots,2,-5));
    assert(contains(roots,2,5));

    // x^3 - 3*x - 2 = (x+1)*(x+1)*(x-2)
    assert(Math::solveCubic(0,-3,-2,roots) == 2);
    assert(contains(roots,2,-1));
    assert(contains(roots,2,2));

    // x^3 - 3*x - 2 = (x+1)*(x+1)*(x-2)
    assert(Math::solveCubic(0,-12,-16,roots) == 2);
    assert(contains(roots,2,-2));
    assert(contains(roots,2,4));

    // x^3 + x^2 = x*x*(x+1)
    assert(Math::solveCubic(1,0,0,roots) == 2);
    assert(contains(roots,2,0));
    assert(contains(roots,2,-1));
    
    // x^3 - x^2 - x + 1= (x+1)*(x-1)*(x-1)
    assert(Math::solveCubic(-1,-1,1,roots) == 2);
    assert(contains(roots,2,1));
    assert(contains(roots,2,-1));

    // x^3 
    assert(Math::solveCubic(0,0,0,roots) == 1);
    assert(contains(roots,1,0));
}

void solve_quadratic_test() {
    double roots[2];
    assert(Math::solveQuadratic(1,0,0,roots) == 1);
    assert(IS_EQUAL(roots[0],0));

    assert(Math::solveQuadratic(1,0,-4,roots) == 2);
    assert(contains(roots,2,2));
    assert(contains(roots,2,-2));

    assert(Math::solveQuadratic(2,0,-8,roots) == 2);
    assert(contains(roots,2,2));
    assert(contains(roots,2,-2));

    assert(Math::solveQuadratic(1,5,6,roots) == 2);
    assert(contains(roots,2,-2));
    assert(contains(roots,2,-3));

    assert(Math::solveQuadratic(2,1,0,roots) == 2);
    assert(contains(roots,2,-0));
    assert(contains(roots,2,-0.5));

    assert(Math::solveQuadratic(5,5,2,roots) == 0);

    assert(Math::solveQuadratic(0,1,-2,roots) == 1);
    assert(contains(roots,1,2));

    assert(Math::solveQuadratic(0,5,10,roots) == 1);
    assert(contains(roots,1,-2));

    assert(Math::solveQuadratic(1,0,-25,roots) == 2);
    assert(contains(roots,2,5));
    assert(contains(roots,2,-5));
}

void perturb_vector_test() {
    double max_angle = DEG2RAD(10.0);
    for(int i = 0; i < 10000; i++) {
	Vector axis = Vector(RANDOM(-1,1),RANDOM(-1,1),RANDOM(-1,1));
	axis.normalize();
	Vector perturbed = Math::perturbVector(axis,max_angle);
	perturbed.normalize();
	double angle = acos(axis*perturbed);
	assert(angle <= max_angle);
    }
}

int main(int argc, char *argv[]) {
    vector_test();
    vector2_test();
    matrix_test();
    binomial_test();
    bernstein_polynomial_test();
    clamp_test();
    test_cubic_root();
    solve_quadratic_test();
    solve_cubic_test();
    solve_quartic_test();
    perturb_vector_test();
    return EXIT_SUCCESS;
}
