
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "testing.h"
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

class test_float_ops : public Test {
    public: 
	void run() {
	    Parser* p = new Parser(getLoadPrefix() + "/scenes/float_ops.gay");
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
	    assert_equal(lookup_float("k3"),-1);
	    assert_equal(lookup_float("l1"),1);
	    assert_equal(lookup_float("l2"),1);
	    assert_equal(lookup_float("m1"),-1);
	    assert_equal(lookup_float("m2"),-3);
	    assert_equal(lookup_float("m3"),-3);
	    assert_equal(lookup_float("n1"),5);
	    assert_equal(lookup_float("n2"),12);
	    assert_equal(lookup_float("o1"),16);
	    assert_equal(lookup_float("o2"),27);
	    assert_equal(lookup_float("p1"),-1);
	    assert_equal(lookup_float("p2"),1);
	    assert_equal(lookup_float("p3"),10);
	    assert_equal(lookup_float("p4"),2);
	    delete p;
	}
};

class test_vector_ops : public Test {
    public: 
	void run() {
	    Parser* p = new Parser(getLoadPrefix() + "/scenes/test_vector_ops.gay");
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
};

class test_functions : public Test {
    public: 
	void run() {
	    Parser* p = new Parser(getLoadPrefix() + "/scenes/functions.gay");
	    p->parse();
	    p->execute();
	    assert_equal(lookup_float("a"),5);

	    delete p;
	}

};

int main(int argc, char *argv[]) {
    TestSuite suite;

    suite.add("Vector operations",new test_vector_ops());
    suite.add("Float operations",new test_float_ops());
    suite.add("Functions",new test_functions());
    suite.run();
    suite.printStatus();
    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}
