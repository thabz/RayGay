
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
    // Test area()
    assert(0.5 == Vector::area(Vector(1,1,1), Vector(2,1,1), Vector(1,2,1)));
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
}

void vector2_test() {
    // Test area()
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

int main(int argc, char *argv[]) {
    vector_test();
    vector2_test();
    matrix_test();
    binomial_test();
    bernstein_polynomial_test();
    clamp_test();
    return EXIT_SUCCESS;
}
