
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <iostream>
#include <cassert>

#include "math/vector.h"
#include "math/matrix.h"

using namespace std;

void vector_test() {
    // Test area()
    assert(0.5 == Vector::area(Vector(1,1,1), Vector(2,1,1), Vector(1,2,1)));

    cout << "vector test() done." << endl;
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

    cout << "matrix test() done." << endl;
}

int main(int argc, char *argv[]) {
    vector_test();
    matrix_test();
    return EXIT_SUCCESS;
}
