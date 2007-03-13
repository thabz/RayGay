
#include "scheme.h"
#include <sstream>

#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

SchemeBool* S_TRUE = new SchemeBool(true);
SchemeBool* S_FALSE = new SchemeBool(false);
SchemeNumber* S_ZERO = new SchemeNumber(0);
SchemeNumber* S_ONE = new SchemeNumber(1);
SchemeNumber* S_TWO = new SchemeNumber(2);
SchemeUnspecified* S_UNSPECIFIED = new SchemeUnspecified();
SchemeEmptyList* S_EMPTY_LIST = new SchemeEmptyList();

Scheme::Scheme() {
    top_level_bindings = new BindingEnvironment(NULL);
	assign("bool?"  ,1,0,0, (SchemeObject* (*)()) s_boolean_p);
	assign("list?"  ,1,0,0, (SchemeObject* (*)()) s_list_p);
	assign("reverse",1,0,0, (SchemeObject* (*)()) s_reverse);
	assign("cons"   ,2,0,0, (SchemeObject* (*)()) s_cons);
	assign("apply"  ,1,0,1, (SchemeObject* (*)()) s_apply);
	assign("map"    ,1,0,1, (SchemeObject* (*)()) s_map);
	assign("display",1,0,0, (SchemeObject* (*)()) s_display);
	assign("newline",1,0,0, (SchemeObject* (*)()) s_newline);
}

SchemeObject* Scheme::eval(istream* is) {
    Lexer* lexer = new Lexer(is);
    Parser* parser = new Parser(lexer);
    SchemePair* parse_tree = parser->parse();
    Interpreter* interpreter = new Interpreter(parse_tree, top_level_bindings);
    return interpreter->interpret();
}

SchemeObject* Scheme::eval(string data) {
    istream* is = new istringstream(data);
    SchemeObject* result = eval(is);
    delete is;
    return result;
};

void Scheme::assign(string variable, int req, int opt, int rst, SchemeObject* (*fn)()) {
    SchemeProcedure* proc = new SchemeProcedure(req, opt, rst, fn);
    top_level_bindings->put(variable, proc);
}

// -----------------------------------------------------
// Procedures
// -----------------------------------------------------

// (boolean? b)
SchemeBool* s_boolean_p(BindingEnvironment* s, SchemeObject* o) {
    return (o == S_TRUE || o == S_FALSE) ? S_TRUE : S_FALSE;
}

// (list? a)
SchemeBool* s_list_p(BindingEnvironment* s, SchemeObject* o) {
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
SchemePair* s_cons(BindingEnvironment* s, SchemeObject* car, SchemeObject* cdr) {
    return new SchemePair(car, cdr);
}

SchemeObject* s_display(BindingEnvironment* s, SchemeObject* o) {
    cout << o->toString();
    return S_UNSPECIFIED;
}

SchemeObject* s_newline(BindingEnvironment* s) {
    cout << endl;        
    return S_UNSPECIFIED;
}

SchemePair* s_reverse(BindingEnvironment* s, SchemeObject* o) {
    SchemePair* result = S_EMPTY_LIST;
    if (s_list_p(s, o) != S_TRUE) {
		throw scheme_exception("reverse with wrong argument");
    }
    while (o != S_EMPTY_LIST) {
		SchemePair* l = static_cast<SchemePair*> (o);
		result = s_cons(s, l->car, result);
		o = l->cdr;
	}
	return result;  
}

SchemeObject* s_apply(BindingEnvironment* s, SchemeProcedure* fn, SchemePair* args) {
}

SchemeObject* s_map(BindingEnvironment* s, SchemeProcedure* fn, SchemePair* args) {
}


SchemeNumber* s_plus(BindingEnvironment* s, SchemePair* p) {
	double result = 0;
	if (p == S_EMPTY_LIST) {
		return S_ZERO;
	}
	while (p != S_EMPTY_LIST) {
		SchemeNumber* n = static_cast<SchemeNumber*>(p->car);
		if (n == NULL) {
			throw scheme_exception("Wrong argument to +: " + p->car->toString());
		}
		result += n->number;
		p = p->cdrAsPair();
	}
	return new SchemeNumber(result);
}

SchemeNumber* s_mult(BindingEnvironment* s, SchemePair* p) {
	double result = 1;
	if (p == S_EMPTY_LIST) {
		return S_ONE;
	}
	
	while (p != S_EMPTY_LIST) {
		SchemeNumber* n = static_cast<SchemeNumber*>(p->car);
		if (n == NULL) {
			throw scheme_exception("Wrong argument to *: " + p->car->toString());
		}
		result *= n->number;
		p = p->cdrAsPair();
	}
	return new SchemeNumber(result);
}


