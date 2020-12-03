
#include "scheme/filenames.h"
#include "scheme/r6rs-lib-io-ports.h"
#include "scheme/scheme.h"

#include "parser/imagefactory.h"
#include "parser/mathfactory.h"
#include "parser/pathfactory.h"

#include "exception.h"

#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <locale>
#include <sstream>
#include <stdexcept>
#include <time.h>
#include <unistd.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if HAVE_LIBREADLINE
#include <readline/history.h>
#include <readline/readline.h>
#endif

using namespace std;

bool verbose = false;
Scheme *scheme;
clock_t elapsed;

#if HAVE_LIBREADLINE
#else
char *readline(const char *prompt) {
  char *input = (char *)malloc(64 * 1024 * sizeof(char));
  cout << "raygay> " << flush;
  cin.getline(input, 64 * 1024);
  if (cin.eof()) {
    return NULL;
  }
  return input;
}
int add_history(const char *line) { return 0; }
int write_history(const char *line) { return 0; }
int read_history(const char *line) { return 0; }
int history_truncate_file(const char *line, int lines) { return 0; }
char *(*rl_completion_entry_function)(const char *, int);
const char *rl_basic_word_break_characters;
#endif

string history_filename() {
  char *home_c = getenv("HOME");
  if (home_c == NULL) {
    return string("");
  } else {
    return string(home_c) + "/.raygay_history";
  }
}

char *binding_completion_function(const char *text, int state) {
  static uint32_t list_index;
  static vector<SchemeObject *> binding_keys;
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
  while (list_index < binding_keys.size()) {
    wstring wkey = binding_keys[list_index]->toString();
    list_index++;
    if (wkey.find(wtext) == 0) {
      string key = prefix + SchemeFilenames::toFilename(wkey);
      return strdup(key.c_str());
    }
  }
  return NULL;
}

int repl(int argc, char *argv[]) {
  try {
    scheme = new Scheme(argc, argv);
    ImageFactory::register_procs(scheme);
    MathFactory::register_procs(scheme);
    PathFactory::register_procs(scheme);
  } catch (scheme_exception e) {
    wcerr << L"ABORT: " << e.toString() << endl;
    return EXIT_FAILURE;
  }

  string history_f = history_filename().c_str();

  if (history_f != "") {
    read_history(history_f.c_str());
  }
  // TODO: Not working on older gettexts, eg. on Mac.
  // rl_completion_entry_function = binding_completion_function;
  rl_basic_word_break_characters = (char *)" \t\n\"'`;()";

  while (true) {
    char *input = ::readline("raygay> ");
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
      SchemeObject *result = scheme->eval(winput);
      if (result != S_UNSPECIFIED) {
        wcout << result->toString() << endl;
      }
    } catch (scheme_exception e) {
      wcerr << L"ABORT: " << e.toString() << endl;
    } catch (exception e) {
      cout << L"ABORT: " << e.what() << endl;
    }
    free(input);
  }
}

int runfile(int argc, char *argv[], char *filename) {
  string s = string(filename);
  wstring wfilename = SchemeFilenames::toString(s);
  try {
    scheme = new Scheme(argc, argv);
    ImageFactory::register_procs(scheme);
    MathFactory::register_procs(scheme);
    PathFactory::register_procs(scheme);

    SchemeObject *transcoder = s_make_transcoder(scheme, s_utf_8_codec(scheme),
                                                 S_UNSPECIFIED, S_UNSPECIFIED);
    SchemeObject *port = s_open_file_input_port(scheme, string2scm(wfilename),
                                                S_FALSE, S_FALSE, transcoder);
    scheme->eval(port);
  } catch (scheme_exception e) {
    // TODO: Close port
    wcerr << L"ABORT: " << e.toString() << endl;
    return EXIT_FAILURE;
  } catch (Exception e) {
    cout << L"ABORT: " << e.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (exception e) {
    cout << L"ABORT: " << e.what() << endl;
    return EXIT_FAILURE;
  }
  // TODO: Close port
  return EXIT_SUCCESS;
}

void print_stats() {
  cout << "------ Stats ------" << endl;

  double secs = double(elapsed) / double(CLOCKS_PER_SEC);
  cout << "Running time            : " << fixed << setprecision(2) << secs
       << "s" << endl;

  Heap *heap = Heap::getUniqueInstance();
  heap->dumpStats();
}

void print_version() { cout << "RayGay Scheme " << VERSION << endl; }

void print_usage() {
  cout << "Usage: repl [OPTION...] [SOURCE-FILE]" << endl;
  cout << "       -d                   Print debugging information" << endl;
  cout << "       -h                   Show this help message" << endl;
  cout << "       -v                   Show version" << endl;
}

void no_mem_handler() {
  cerr << "Out of memory" << endl;
  throw std::bad_alloc();
}

int main(int argc, char *argv[]) {

  // std::set_new_handler(&no_mem_handler);

  // Use getopt to parse arguments.
  int c;
  opterr = 0;
  while ((c = getopt(argc, argv, "hvd")) != -1) {
    switch (c) {
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
    result = runfile(argc, argv, argv[optind]);
  } else {
    result = repl(argc, argv);
  }

  elapsed = clock() - elapsed;

  if (verbose) {
    print_stats();
  }

  return result;
}
