
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "parser/parserclass.h"
#include "parser/assignments.h"
#include <cstdlib>
#include <cmath>
#include <cassert>
#include <iostream>

using namespace std;

#define lookup_float(s) Assignments::getUniqueInstance()->getNamedFloat(s)
#define lookup_vector(s) Assignments::getUniqueInstance()->getNamedVector(s)
#define assert_equal(a,b) assert(IS_EQUAL((a),(b)))

void test_float_ops() {
    Parser p = Parser("scenes/float_ops.gay");
    p.parse();
    p.execute();
    assert_equal(lookup_float("a"),1);
    assert_equal(lookup_float("b"),2);
    cout << lookup_float("d") << endl;
    assert_equal(lookup_float("d"),2);
    cout << lookup_float("c") << endl;
    assert_equal(lookup_float("c"),3);
}

void test_vector_ops() {
    Parser p = Parser("scenes/test_vector_ops.gay");
    p.parse();
    p.execute();
    assert(lookup_vector("a") == Vector(1,1,1));
    assert(lookup_vector("b") == Vector(1,2,3));
    assert_equal(lookup_float("c"),sqrt(1.0 + 4.0 + 9.0));
}

int main(int argc, char *argv[]) {
    test_vector_ops();
    test_float_ops();
    return EXIT_SUCCESS;
}
