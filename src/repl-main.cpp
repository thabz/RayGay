
#include "scheme/scheme.h"
#include "scheme/filenames.h"

#include "parser/imagefactory.h"
#include "parser/mathfactory.h"

#include "exception.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <time.h>
#include <locale>
#include <stdexcept>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

using namespace std;

bool verbose = false;
Scheme* scheme;
clock_t elapsed;

int repl() {
    wchar_t input[64*1024];
    
    try {
        scheme = new Scheme();
        ImageFactory::register_procs(scheme);
        MathFactory::register_procs(scheme);
    } catch (scheme_exception e) {
	wcerr << L"ABORT: " << e.toString() << endl;
        return EXIT_FAILURE;
    }

    while (true) {
        cout << "raygay> " << flush;
        wcin.getline(input, 64*1024);
        if (wcin.eof()) {
    	    // User pressed ctrl-D.
    	    return EXIT_SUCCESS;
        } 
           
        try {
           SchemeObject* result = scheme->eval(wstring(input));
           if (result != S_UNSPECIFIED) {
               wcout << result->toString() << endl;
           }
        } catch (scheme_exception e) {
    	   wcerr << L"ABORT: " << e.toString() << endl;
        }
    }
}

int runfile(char* filename) {
    wifstream* ifs = new wifstream(filename, ios::in);
    try {
        std::locale loc("");
        ifs->imbue(loc);
    } catch (std::runtime_error e) {
        cout << "Warning: can't read system locale. UTF-8 files won't be read correctly." << endl;
    }
    
    if (ifs->fail()) {
        cout << "Error opening file" << endl;
        return EXIT_FAILURE;
    }
    try {
        scheme = new Scheme();
        ImageFactory::register_procs(scheme);
        MathFactory::register_procs(scheme);
        scheme->eval(ifs);
    } catch (scheme_exception e) {
        ifs->close();
	wcerr << L"ABORT: " << e.toString() << endl;
        return EXIT_FAILURE;
    } catch (Exception e) {
	cout << L"ABORT: " << e.getMessage() << endl;

    }
    ifs->close();
    return EXIT_SUCCESS;
}

void print_stats() {
    cout << "------ Stats ------" << endl;       

    double secs = double(elapsed) / double(CLOCKS_PER_SEC);
    cout << "Running time            : " << fixed << setprecision(2) << secs << "s" << endl;
    
    Heap* heap = Heap::getUniqueInstance();
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
    
    elapsed = clock();
    
    int result;
    if (optind == argc - 1) {
        result = runfile(argv[optind]);
    } else {
        result = repl();
    }    
    
    elapsed = clock() - elapsed;
    
    if (verbose) {
        print_stats();    
    }
    
    return result;
}
