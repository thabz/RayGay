
#include "scheme.h"
#include <iostream>
#include <sstream>
#include <fstream>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

using namespace std;

bool verbose = false;
Scheme* scheme;

int repl() {
    char input[64*1024];
    
    try {
        scheme = new Scheme();
    } catch (scheme_exception e) {
	cerr << "ABORT: " << e.toString() << endl;
        return EXIT_FAILURE;
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
    	   cerr << "ABORT: " << e.toString() << endl;
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
        scheme = new Scheme();
        scheme->eval(ifs);
    } catch (scheme_exception e) {
        ifs->close();
	cerr << "ABORT: " << e.toString() << endl;
        return EXIT_FAILURE;
    }
    ifs->close();
    return EXIT_SUCCESS;
}

void print_stats() {
    Heap* heap = Heap::getUniqueInstance();
    cout << "------ Stats ------" << endl;       
    heap->dumpStats();    
}

void print_version() {
    cout << "RayGay Scheme " << VERSION << endl;        
}

void print_usage() {
    cout << "Usage: repl [OPTION...] [SOURCE-FILE]" << endl;
    cout << "       -d                   Print debugging information" << endl;
    cout << "       -h                   Show this help message" << endl;
    cout << "       -v                   Show version" << endl;
}

int main(int argc, char *argv[]) {
    
    // Use getopt to parse arguments.
    int c;
    opterr = 0;
    while ((c = getopt (argc, argv, "hvd")) != -1) {
	switch(c) {
	    case 'h':
		print_usage();
		return EXIT_SUCCESS;
	    case 'v':
		print_version();
		return EXIT_SUCCESS;
	    case 'd':
		verbose = true;
		break;
	    case '?':
		cerr << "Unknown option -" << char(optopt) << endl << endl;
		print_usage();
		return EXIT_FAILURE;
	    default:
		return EXIT_FAILURE;
	}
    }        
    
    int result;

    if (optind == argc - 1) {
        result = runfile(argv[optind]);
    } else {
        result = repl();
    }    
    
    if (verbose) {
        print_stats();    
    }
    
    return result;
}
