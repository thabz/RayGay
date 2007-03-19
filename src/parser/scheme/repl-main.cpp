
#include "scheme.h"
#include <iostream>
#include <sstream>

using namespace std;

int main(int argc, char *argv[]) {
    Scheme* scheme = new Scheme();
	char input[64*1024];
	
	while (true) {
 	    cout << "raygay> " << flush;
	    cin.getline(input, 64*1024);
	    if (cin.eof()) {
		// User pressed ctrl-D.
		return EXIT_SUCCESS;
	    }    
	    try {
               SchemeObject* result = scheme->eval(string(input));
           if (result != S_UNSPECIFIED) {
	           cout << result->toString() << endl;
           }
	    } catch (scheme_exception e) {
			cerr << e.str << endl;
			return EXIT_FAILURE;
 	    }
	}
}
