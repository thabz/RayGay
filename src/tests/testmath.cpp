
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
}

int main(int argc, char *argv[]) {
    vector_test();
    vector2_test();
    matrix_test();
    return EXIT_SUCCESS;
}
