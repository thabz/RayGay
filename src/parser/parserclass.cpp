
#include <cstdlib>

#include "parserclass.h"

#include "parser/assignments.h"    
#include "parser/floatnodes.h"    
#include "parser/syntaxnode.h"    
#include "parser/vectornodes.h"    
#include "parser/rgbnodes.h"    
#include "parser/langnodes.h"    
#include "parser/transformationnodes.h"    
#include "parser/lightnodes.h"    
#include "parser/cameranode.h"    
#include "parser/parser.h"

#include "renderersettings.h"

using namespace std;

extern FILE* yyin;
extern void yyparse();
extern void run_interpreter();
extern void delete_interpreter();
extern void init_parser(string filename);
extern RendererSettings* getRendererSettings();

Parser::Parser(std::string filename) {
    this->filename = filename;
}

Parser::~Parser() {
    delete_interpreter();
}

void Parser::parse() {
    char original_working_dir[1024];
    getcwd(original_working_dir,1024);

    init_parser(filename);
    yyparse();

    chdir(original_working_dir);
}

void Parser::execute() {
    run_interpreter();
}

