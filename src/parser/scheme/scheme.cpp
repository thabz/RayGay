
#include "scheme.h"
#include <sstream>

#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

SchemeBool* Scheme::S_TRUE = new SchemeBool(true);
SchemeBool* Scheme::S_FALSE = new SchemeBool(false);
SchemeNumber* Scheme::S_ZERO = new SchemeNumber(0);
SchemeNumber* Scheme::S_ONE = new SchemeNumber(1);
SchemeUnspecified* Scheme::S_UNSPECIFIED = new SchemeUnspecified();
SchemeEmptyList* Scheme::S_EMPTY_LIST = new SchemeEmptyList();

Scheme::Scheme() {
}

SchemeObject* Scheme::eval(istream* is) {
    Lexer* lexer = new Lexer(is);
    Parser* parser = new Parser(lexer, this);
    SchemeObject* parse_tree = parser->parse();
    Interpreter* interpreter = new Interpreter(parse_tree, this);
    return interpreter->interpret();
}

SchemeObject* Scheme::eval(string data) {
    istream* is = new istringstream(data);
    SchemeObject* result = eval(is);
    delete is;
    return result;
};

void Scheme::die_with_error(string error) {
    cerr << error << endl;
    exit(EXIT_FAILURE);
}

// -----------------------------------------------------
// Procedures
// -----------------------------------------------------

// (boolean? b)
SchemeBool* Scheme::boolean_p(SchemeObject* o) {
    return (o == S_TRUE || o == S_FALSE) ? S_TRUE : S_FALSE;
}

// (list? a)
SchemeBool* Scheme::list_p(SchemeObject* o) {
    if (o == S_EMPTY_LIST) {
        return S_TRUE;
    }
    SchemePair* p = static_cast<SchemePair*> (o);
    if (p != NULL) {
        SchemeObject* cdr = p->cdr;
        if (cdr == S_EMPTY_LIST) {
            return S_TRUE;
        }
        SchemePair* p2 = static_cast<SchemePair*> (cdr);
        return p2 != NULL ? S_TRUE : S_FALSE;
        
    }
    return S_FALSE;
}

// (cons a b)
SchemePair* Scheme::cons(SchemeObject* car, SchemeObject* cdr) {
    return new SchemePair(car, cdr);
}

SchemeObject* Scheme::display(SchemeObject* o) {
    cout << o->toString();
    return S_UNSPECIFIED;
}

SchemeObject* Scheme::newline() {
    cout << endl;        
    return S_UNSPECIFIED;
}

SchemeObject* Scheme::reverse(SchemeObject* o) {
    SchemeObject* result = S_EMPTY_LIST;
    if (list_p(o) != S_TRUE) {
		die_with_error("reverse with wrong argument");
    }
    while (o != S_EMPTY_LIST) {
		SchemePair* l = static_cast<SchemePair*> (o);
		result = cons(l->car, result);
		o = l->cdr;
	}
	return result;  
}