
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "testing.h"
#include "parser/parser.h"
#include <cstdlib>
#include <cmath>
#include <cassert>
#include <iostream>
#include <libguile.h>

using namespace std;

#define lookupDouble(s) scm_num2double(scm_variable_ref(scm_c_lookup(s)),0,"")
#define lookupVector(s) scm2vector(scm_variable_ref(scm_c_lookup(s)),0,"")

class test_parser : public Test {
    public: 
	void run() {
	    Parser* p = new Parser(getLoadPrefix() + "/scenes/test.scm");
	    
	    scm_c_define("test-predefined-a", scm_double2num(30));
	    p->run();

	    assertTrue(IS_EQUAL(lookupDouble("test-predefined-a"),30));
	    assertTrue(IS_EQUAL(lookupDouble("test-define-1"),10));

	    assertTrue(IS_EQUAL(lookupDouble("test-vdot-1"),4 + 10 + 18));
	    assertTrue(IS_EQUAL(lookupDouble("test-vdot-2"),12 - 5 + 12));

	    assertTrue(IS_EQUAL(lookupDouble("test-vlength-1"),sqrtl(1.0+4+9)));
	    assertTrue(IS_EQUAL(lookupDouble("test-vlength-2"),sqrtl(16+4+4)));
	    delete p;
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
