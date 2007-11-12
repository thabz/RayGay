
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
#include <cstdlib>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if HAVE_LIBREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

using namespace std;

bool verbose = false;
Scheme* scheme;
clock_t elapsed;

#if HAVE_LIBREADLINE
#else
char* readline(const char* prompt) {
    char* input = (char*) malloc(64*1024*sizeof(char));
    cout << "raygay> " << flush;
    cin.getline(input, 64*1024);
    if (cin.eof()) {
        return NULL;
    } 
    return input;
}
int add_history(const char* line) { }
int write_history(const char* line) { }
int read_history(const char* line) { }
int truncate_history_file(const char* line, int lines) { }
#endif

string history_filename() {
    char* home_c = getenv("HOME");
    if (home_c == NULL) {
        return string("");
    } else {
        return string(home_c) + "/.raygay_history";
    }
}

char* binding_completion_function(const char* text, int state) {
    static uint list_index;
    static vector<SchemeObject*> binding_keys;
    if (!state) {
        list_index = 0;
        binding_keys = scheme->getInteractionEnvironment()->getBindingKeys();
    }
    wstring wtext = SchemeFilenames::toString(string(text));
    string prefix = "";
    if (wtext[0] == L'(') {
        prefix = "(";
        wtext = wtext.substr(1);
    }
    while(list_index < binding_keys.size()) {
        wstring wkey = binding_keys[list_index]->toString();
        list_index++;
        if (wkey.find(wtext) == 0) {
            string key = prefix + SchemeFilenames::toFilename(wkey);
            return strdup(key.c_str());
        }
    }
    return NULL;
}

int repl() {
    try {
        scheme = new Scheme();
        ImageFactory::register_procs(scheme);
        MathFactory::register_procs(scheme);
    } catch (scheme_exception e) {
	    wcerr << L"ABORT: " << e.toString() << endl;
        return EXIT_FAILURE;
    }
    
    string history_f = history_filename().c_str();

    
    if (history_f != "") {
        read_history(history_f.c_str());
    }
    rl_completion_entry_function = binding_completion_function;

    while (true) {
        char* input = ::readline("raygay> ");
        if (input == NULL) {
    	    // We got an EOF, because the user pressed ctrl-D.
    	    if (history_f != "") {
                write_history(history_f.c_str());
                history_truncate_file(history_f.c_str(), 1000);
            }
            return EXIT_SUCCESS;
        }
        if (*input != '\0') {
            ::add_history(input);
        }   
        try {
            wstring winput = SchemeFilenames::toString(string(input));
            SchemeObject* result = scheme->eval(winput);
            if (result != S_UNSPECIFIED) {
                wcout << result->toString() << endl;
            }
        } catch (scheme_exception e) {
    	    wcerr << L"ABORT: " << e.toString() << endl;
        }
        free(input);
    }
    
}

int runfile(char* filename) {
    wifstream* ifs = new wifstream(filename, ios::in);
    try {
        //std::locale loc("");
        ifs->imbue(wcout.getloc());
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
