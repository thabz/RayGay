
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <iostream>
#include <cassert>

#include "math/vector.h"
#include "math/vector2.h"
#include "math/matrix.h"

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
}

void vector2_test() {
    // Test area()
    Vector2 v = Vector2(1.0,2.0);
    assert(v[0] == 1);
    assert(v[1] == 2);
    v =  v * 2.0;
    Vector2 w = Vector2(2.0,4.0);
    assert(v == w);
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
}

int main(int argc, char *argv[]) {
    vector_test();
    vector2_test();
    matrix_test();
    return EXIT_SUCCESS;
}
