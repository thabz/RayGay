
#include "scheme.h"
#include <iostream>
#include <sstream>
#include <fstream>

using namespace std;

int repl() {
	char input[64*1024];
    Scheme* scheme;
    
	try {
        scheme = new Scheme();
    } catch (scheme_exception e) {
		cerr << "ABORT: " << e.str << endl;
    }

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
			cerr << "ABORT: " << e.str << endl;
 	    }
	}
}

int runfile(char* filename) {
    ifstream* ifs = new ifstream(filename, ios::in);
    if (ifs->fail()) {
        cout << "Error opening file" << endl;
        return EXIT_FAILURE;
    }
    try {
        Scheme* scheme = new Scheme();
        scheme->eval(ifs);
    } catch (scheme_exception e) {
        ifs->close();
		cerr << "ABORT: " << e.str << endl;
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
