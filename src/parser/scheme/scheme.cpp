
#include "scheme.h"
#include <sstream>

#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

Scheme::Scheme() {
    
}

SchemeObject Scheme::eval(istream* is) {
    Lexer* lexer = new Lexer(is);
    Parser* parser = new Parser(lexer);
    SchemePair parseTree = parser->parse();
    Interpreter* interpreter = new Interpreter(parseTree);
    return interpreter->interpret();
}


SchemeObject Scheme::eval(string data) {
    istream* is = new istringstream(data);
    SchemeObject result = eval(is);
    delete is;
    return result;
};


