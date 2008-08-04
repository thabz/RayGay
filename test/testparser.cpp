
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "testing.h"
#include "parser/sceneparser.h"
#include "parser/converters.h"
#include <cstdlib>
#include <cmath>
#include <cassert>
#include <iostream>
#include "scheme/scheme.h"
#include "scene.h"

using namespace std;

#define lookupDouble(s) safe_scm2double(p->lookup(s),0,L"")
#define lookupVector(s) scm2vector(p->lookup(s),L"",0)
#define lookupRGBA(s) scm2rgba(p->lookup(s),L"",0)

class test_parser : public Test {
    private:
	Vector v1;
    public: 
	void run() {
        Scene* scene = new Scene();        
	    SceneParser* p = new SceneParser(scene);

        p->assignVariable(L"test-predefined-a", 30);
	    p->parse_file(SchemeFilenames::toString(getLoadPrefix() + "/scenes/test.scm"));

	    assertTrue(IS_EQUAL(lookupDouble(L"test-predefined-a"),30));
	    assertTrue(IS_EQUAL(lookupDouble(L"test-define-1"),10));

	    assertTrue(IS_EQUAL(lookupDouble(L"test-vdot-1"),4 + 10 + 18));
	    assertTrue(IS_EQUAL(lookupDouble(L"test-vdot-2"),12 - 5 + 12));

	    assertTrue(IS_EQUAL(lookupDouble(L"test-vlength-1"),sqrt(double(1.0+4+9))));
	    assertTrue(IS_EQUAL(lookupDouble(L"test-vlength-2"),sqrt(double(16.0+4+4))));

	    assertTrue(lookupVector(L"test-vscale-1") == Vector(2,4,6));
	    assertTrue(lookupVector(L"test-vscale-2") == Vector(12,-6,6));

	    assertTrue(lookupVector(L"test-vplus-1") == Vector(3,7,12));
	    assertTrue(lookupVector(L"test-vplus-2") == Vector(8,3,8));

	    assertTrue(lookupVector(L"test-vminus-1") == Vector(-1,-3,-6));
	    assertTrue(lookupVector(L"test-vminus-2") == Vector(0,-7,-4));

	    v1 = Vector(1,2,3);
	    v1.normalize();
	    assertTrue(lookupVector(L"test-vnormalize-1") == v1);
	    v1 = Vector(4,-2,2);
	    v1.normalize();
	    assertTrue(lookupVector(L"test-vnormalize-2") == v1);

	    assertTrue(lookupVector(L"test-translate-1") == Vector(2,3,4));
	    assertTrue(lookupVector(L"test-translate-2") == Vector(1,0,-1));

	    assertTrue(lookupVector(L"test-rotate-1") == Vector(0,-1,0));
	    assertTrue(lookupVector(L"test-rotate-2") == Vector(1,0,0));

	    // RGBA tests
	    assertTrue(lookupRGBA(L"color-white") == RGBA(1.,1,1,1));
	    assertTrue(lookupRGBA(L"color-white") == RGB(1.,1,1));
	    assertTrue(lookupRGBA(L"color-blue-trans") == RGBA(0,0,1,0.5));
	}
};

int main(int argc, char *argv[]) {
    TestSuite suite;

    suite.add("Parser",new test_parser());
    try {
        suite.run();
    } catch (scheme_exception e) {
        wcerr << e.toString() << endl;    
    }        
    suite.printStatus();
    if (suite.hasFailures()) {
	return EXIT_FAILURE;
    } else {
	return EXIT_SUCCESS;
    }
}
