
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "testing.h"
#include "parser/parser.h"
#include "parser/converters.h"
#include <cstdlib>
#include <cmath>
#include <cassert>
#include <iostream>
#include <libguile.h>

using namespace std;

#define lookupDouble(s) scm_num2double(scm_variable_ref(scm_c_lookup(s)),0,"")
#define lookupVector(s) scm2vector(scm_variable_ref(scm_c_lookup(s)),"",0)
#define lookupRGBA(s) scm2rgba(scm_variable_ref(scm_c_lookup(s)),"",0)

class test_parser : public Test {
    private:
	Vector v1;
    public: 
	void run() {
	    Parser* p = new Parser();
	    scm_c_define("test-predefined-a", scm_double2num(30));
	    p->parse_file(getLoadPrefix() + "/scenes/test.scm");

	    assertTrue(IS_EQUAL(lookupDouble("test-predefined-a"),30));
	    assertTrue(IS_EQUAL(lookupDouble("test-define-1"),10));

	    assertTrue(IS_EQUAL(lookupDouble("test-vdot-1"),4 + 10 + 18));
	    assertTrue(IS_EQUAL(lookupDouble("test-vdot-2"),12 - 5 + 12));

	    assertTrue(IS_EQUAL(lookupDouble("test-vlength-1"),sqrt(double(1.0+4+9))));
	    assertTrue(IS_EQUAL(lookupDouble("test-vlength-2"),sqrt(double(16.0+4+4))));

	    assertTrue(lookupVector("test-vscale-1") == Vector(2,4,6));
	    assertTrue(lookupVector("test-vscale-2") == Vector(12,-6,6));

	    assertTrue(lookupVector("test-vplus-1") == Vector(3,7,12));
	    assertTrue(lookupVector("test-vplus-2") == Vector(8,3,8));

	    assertTrue(lookupVector("test-vminus-1") == Vector(-1,-3,-6));
	    assertTrue(lookupVector("test-vminus-2") == Vector(0,-7,-4));

	    v1 = Vector(1,2,3);
	    v1.normalize();
	    assertTrue(lookupVector("test-vnormalize-1") == v1);
	    v1 = Vector(4,-2,2);
	    v1.normalize();
	    assertTrue(lookupVector("test-vnormalize-2") == v1);
	    delete p;

	    assertTrue(lookupVector("test-translate-1") == Vector(2,3,4));
	    assertTrue(lookupVector("test-translate-2") == Vector(1,0,-1));

	    assertTrue(lookupVector("test-rotate-1") == Vector(0,-1,0));
	    assertTrue(lookupVector("test-rotate-2") == Vector(1,0,0));

	    // RGBA tests
	    assertTrue(lookupRGBA("color-white") == RGBA(1.,1,1,1));
	    assertTrue(lookupRGBA("color-white") == RGB(1.,1,1));
	    assertTrue(lookupRGBA("color-blue-trans") == RGBA(0,0,1,0.5));
	}
};

int main(int argc, char *argv[]) {
    TestSuite suite;

    suite.add("Parser",new test_parser());
    suite.run();
    suite.printStatus();
    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}
