
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
		try {
           SchemeObject* result = scheme->eval(string(input));
	       cout << result->toString() << endl;
	    } catch (scheme_exception e) {
			cerr << e.str << endl;
 	    }
	}
}
