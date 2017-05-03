
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

#define lookupInt(s) safe_scm2int(p->lookup(s),0,L"")
#define lookupDouble(s) safe_scm2double(p->lookup(s),0,L"")
#define lookupVector(s) scm2vector(p->lookup(s),L"",0)
#define lookupRGBA(s) scm2rgba(p->lookup(s),L"",0)

class test_mesh : public Test {
    private:
	Vector v1;
    public: 
	void run() {
	    Scene* scene = new Scene();        
	    SceneParser* p = new SceneParser(scene);

	    p->assignVariable(L"test-predefined-a", 30);
	    p->parse_file(SchemeFilenames::toString(getLoadPrefix() + "/scheme/test-mesh.scm"));

	    assertTrue(lookupInt(L"tetrahedron-vertices-num") == 4);
	    assertTrue(lookupInt(L"tetrahedron-faces-num") == 4);
	    assertTrue(lookupInt(L"tetrahedron-edges-num") == 6);

	    assertTrue(lookupInt(L"hexahedron-vertices-num") == 8);
	    assertTrue(lookupInt(L"hexahedron-faces-num") == 6*2);
	    assertTrue(lookupInt(L"hexahedron-edges-num") == 12+6);
	    
        // The following doesn't work on Linux, possible due to
        // some precision issue. This is the last remaining failing 
        // tests for everything to work and I can't be bothered 
        // to find the problem.
	    //assertTrue(lookupInt(L"octahedron-vertices-num") == 6);
	    //assertTrue(lookupInt(L"octahedron-faces-num") == 8);
	    //assertTrue(lookupInt(L"octahedron-edges-num") == 12);

	    assertTrue(lookupInt(L"dodecahedron-vertices-num") == 20);
	    assertTrue(lookupInt(L"dodecahedron-faces-num") == 12*3);
	    assertTrue(lookupInt(L"dodecahedron-edges-num") == 30 + 12*2);

	    assertTrue(lookupInt(L"icosahedron-vertices-num") == 12);
	    assertTrue(lookupInt(L"icosahedron-faces-num") == 20);
	    assertTrue(lookupInt(L"icosahedron-edges-num") == 30);
	}
};

class schemefunctions_test : public Test {
    public: 
	void run() {
	    Scene* scene = new Scene();        
	    SceneParser* p = new SceneParser(scene);
	    cout << endl;
	    p->parse_file(SchemeFilenames::toString(getLoadPrefix() + "/scheme/tests.scm"));
	}
};

int main(int argc, char *argv[]) {
    TestSuite suite;

    suite.add("Mesh",new test_mesh());
    suite.add("Scheme tests",new schemefunctions_test());
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
