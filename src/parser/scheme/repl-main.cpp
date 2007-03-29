
#include "scheme.h"
#include <iostream>
#include <sstream>
#include <fstream>

using namespace std;

int repl() {
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
 	    }
	}
}

int runfile(char* filename) {
    Scheme* scheme = new Scheme();
    ifstream* ifs = new ifstream(filename, ios::in);
    if (ifs->fail()) {
        cout << "Error opening file" << endl;
        return EXIT_FAILURE;
    }
    try {
        scheme->eval(ifs);
    } catch (scheme_exception e) {
        ifs->close();
		cerr << e.str << endl;
        return EXIT_FAILURE;
    }
    ifs->close();
    return EXIT_SUCCESS;
}

int main(int argc, char *argv[]) {
    if (argc == 2) {
        return runfile(argv[1]);
    } else {
        return repl();
    }    
}
