
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "parser/parserclass.h"
#include "parser/assignments.h"
#include "parser/fileposition.h"
#include <cstdlib>
#include <cmath>
#include <cassert>
#include <iostream>

using namespace std;

FilePosition pos;

#define lookup_float(s) Assignments::getUniqueInstance()->getNamedFloat(s,pos)
#define lookup_vector(s) Assignments::getUniqueInstance()->getNamedVector(s,pos)
#define assert_equal(a,b) assert(IS_EQUAL((a),(b)))

void test_float_ops() {
    Parser* p = new Parser("scenes/float_ops.gay");
    p->parse();
    p->execute();
    assert_equal(lookup_float("a"),1);
    assert_equal(lookup_float("b"),2);
    assert_equal(lookup_float("c"),3);
    assert_equal(lookup_float("d"),2);
    assert_equal(lookup_float("e"),11);
    assert_equal(lookup_float("f"),30);
    assert_equal(lookup_float("g"),-50);
    assert_equal(lookup_float("h"),9);
    assert_equal(lookup_float("i"),8);
    assert_equal(lookup_float("j1"),1);
    assert_equal(lookup_float("j2"),0);
    assert_equal(lookup_float("k1"),-1);
    assert_equal(lookup_float("k2"),1);
    assert_equal(lookup_float("l1"),1);
    assert_equal(lookup_float("l2"),1);
    delete p;
}

void test_vector_ops() {
    Parser* p = new Parser("scenes/test_vector_ops.gay");
    p->parse();
    p->execute();
    assert(lookup_vector("a") == Vector(1,1,1));
    assert(lookup_vector("b") == Vector(1,2,3));
    assert_equal(lookup_float("c"),sqrt(1.0 + 4.0 + 9.0));
    assert(lookup_vector("d") == Vector(2,3,4));
    assert(lookup_vector("e") == Vector(2,2,2));
    assert(lookup_vector("f") == Vector(2,4,6));
    assert(lookup_vector("h") == Vector(3,1,2));
    assert(lookup_vector("i") == Vector(10,10,10));
    assert(lookup_vector("j") == Vector(4,1,2));
    delete p;
}

void test_functions() {
    Parser* p = new Parser("scenes/functions.gay");
    p->parse();
    p->execute();
    assert_equal(lookup_float("a"),5);

    delete p;

}

int main(int argc, char *argv[]) {
    test_vector_ops();
    test_float_ops();
    test_functions();
    return EXIT_SUCCESS;
}
