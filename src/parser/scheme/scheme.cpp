
#include "scheme.h"
#include <sstream>
#include <fstream>

#include "lexer.h"
#include "parser.h"
#include "interpreter.h"

SchemeBool* S_TRUE = new SchemeBool(true);
SchemeBool* S_FALSE = new SchemeBool(false);
SchemeNumber* S_ZERO = new SchemeNumber(0);
SchemeNumber* S_ONE = new SchemeNumber(1);
SchemeNumber* S_TWO = new SchemeNumber(2);
SchemeNumber* S_THREE = new SchemeNumber(3);
SchemeNumber* S_FOUR = new SchemeNumber(4);
SchemeNumber* S_FIVE = new SchemeNumber(5);
SchemeNumber* S_SIX = new SchemeNumber(6);
SchemeNumber* S_SEVEN = new SchemeNumber(7);
SchemeNumber* S_EIGHT = new SchemeNumber(8);
SchemeNumber* S_NINE = new SchemeNumber(9);
SchemeUnspecified* S_UNSPECIFIED = new SchemeUnspecified();
SchemeEmptyList* S_EMPTY_LIST = new SchemeEmptyList();
SchemeNumber* S_NUMBERS[] = {S_ZERO, S_ONE, S_TWO, S_THREE, S_FOUR, S_FIVE, S_SIX, S_SEVEN, S_EIGHT, S_NINE};

Scheme::Scheme() {
    top_level_bindings = new BindingEnvironment(NULL);
	assign("equal?"     ,2,0,0, (SchemeObject* (*)()) s_equal_p);
	assign("bool?"      ,1,0,0, (SchemeObject* (*)()) s_boolean_p);
	assign("pair?"      ,1,0,0, (SchemeObject* (*)()) s_pair_p);
	assign("symbol?"    ,1,0,0, (SchemeObject* (*)()) s_symbol_p);
	assign("list?"      ,1,0,0, (SchemeObject* (*)()) s_list_p);
	assign("string?"    ,1,0,0, (SchemeObject* (*)()) s_string_p);
	assign("procedure?" ,1,0,0, (SchemeObject* (*)()) s_procedure_p);
	assign("number?"    ,1,0,0, (SchemeObject* (*)()) s_number_p);
	assign("vector?"    ,1,0,0, (SchemeObject* (*)()) s_vector_p);
	assign("car"        ,1,0,0, (SchemeObject* (*)()) s_car);
	assign("cdr"        ,1,0,0, (SchemeObject* (*)()) s_cdr);
	assign("list"       ,0,0,1, (SchemeObject* (*)()) s_list);
	assign("list-tail"  ,2,0,0, (SchemeObject* (*)()) s_list_tail);
	assign("list-ref"   ,2,0,0, (SchemeObject* (*)()) s_list_ref);
	assign("member"     ,2,0,0, (SchemeObject* (*)()) s_member);
	assign("reverse"    ,1,0,0, (SchemeObject* (*)()) s_reverse);
	assign("length"     ,1,0,0, (SchemeObject* (*)()) s_length);
	assign("cons"       ,2,0,0, (SchemeObject* (*)()) s_cons);
	assign("display"    ,1,0,0, (SchemeObject* (*)()) s_display);
	assign("newline"    ,0,0,0, (SchemeObject* (*)()) s_newline);
	assign("+"          ,0,0,1, (SchemeObject* (*)()) s_plus);
	assign("-"          ,0,0,1, (SchemeObject* (*)()) s_minus);
	assign("*"          ,0,0,1, (SchemeObject* (*)()) s_mult);
	assign("make-vector",2,0,0, (SchemeObject* (*)()) s_make_vector);
	assign("vector"     ,0,0,1, (SchemeObject* (*)()) s_vector);
	assign("vector-length",1,0,0, (SchemeObject* (*)()) s_vector_length);
	assign("vector-ref" ,2,0,0, (SchemeObject* (*)()) s_vector_ref);
	assign("vector-set!",3,0,0, (SchemeObject* (*)()) s_vector_set_e);
	assign("list->vector",1,0,0, (SchemeObject* (*)()) s_list_2_vector);
	assign("vector->list",1,0,0, (SchemeObject* (*)()) s_vector_2_list);
	
    ifstream infile;
    infile.open("init.scm", ifstream::in);
    //eval(&infile);
    infile.close();
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
    top_level_bindings->put(SchemeSymbol::create(variable), proc);
}

// -----------------------------------------------------
// Procedures
// -----------------------------------------------------

inline
SchemeNumber* make_number(int n) {
    if (n < 10 && n >= 0) {
        return S_NUMBERS[n];
    }
    return new SchemeNumber(n);
}
// (equal? a b)
// Equal? recursively compares the contents of pairs, vectors, and strings, applying eqv? on other objects 
// such as numbers and symbols. A rule of thumb is that objects are generally equal? if they print the same. 
// Equal? may fail to terminate if its arguments are circular data structures.
SchemeBool* s_equal_p(SchemeObject* a, SchemeObject* b) {
    return a->toString() == b->toString() ? S_TRUE : S_FALSE; 
}

// (boolean? b)
SchemeBool* s_boolean_p(SchemeObject* o) {
    return (o == S_TRUE || o == S_FALSE) ? S_TRUE : S_FALSE;
}

// (list? a)
SchemeBool* s_list_p(SchemeObject* o) {
    if (o == S_EMPTY_LIST) {
        return S_TRUE;
    }
    SchemePair* p = static_cast<SchemePair*> (o);
    if (p != NULL) {
        while (s_pair_p(p) == S_TRUE && p != S_EMPTY_LIST) {
            p = p->cdrAsPair();
            if (p == NULL) {
                return S_FALSE;
            }
        }
        return p == S_EMPTY_LIST ? S_TRUE : S_FALSE;    
    } else {
        return S_FALSE;
    }
}

SchemeObject* s_member(SchemeObject* obj, SchemePair* p) {
    while (p != S_EMPTY_LIST) {
        if (s_equal_p(obj, p->car) == S_TRUE) {
            return p;
        } else {
            p = p->cdrAsPair();
        }
    }
    return S_FALSE;
}

SchemePair* s_list_tail(SchemePair* l, SchemeNumber* k) {
    int i = int(k->number);
    if (i < 0) {
        throw scheme_exception("Index out of range: " + k->toString());
    }
    while (i-- > 0) {
        if (l == S_EMPTY_LIST) {
            throw scheme_exception("Index out of range: " + k->toString());
        }
        l = l->cdrAsPair();
    }
    //if (i != 0) {
    //    // Catches SchemeNumber being a non-integer
    //    throw scheme_exception("Index out of range: " + k->toString());
    //}
    return l;
}

SchemeObject* s_list_ref(SchemePair* l, SchemeNumber* index) {
    SchemePair* p = s_list_tail(l, index);
    if (p == S_EMPTY_LIST) {
        throw scheme_exception("Index out of range: " + index->toString());
    }
    return p->car;
}

// (pair? p)
SchemeBool* s_pair_p(SchemeObject* p) {
    return (p->type() == SchemeObject::PAIR) ? S_TRUE : S_FALSE;
}

// (symbol? p)
SchemeBool* s_symbol_p(SchemeObject* p) {
    return (p->type() == SchemeObject::SYMBOL) ? S_TRUE : S_FALSE;
}

// (string? p)
SchemeBool* s_string_p(SchemeObject* p) {
    return (p->type() == SchemeObject::STRING) ? S_TRUE : S_FALSE;
}

// (procedure? p)
SchemeBool* s_procedure_p(SchemeObject* p) {
    return (p->type() == SchemeObject::PROCEDURE) ? S_TRUE : S_FALSE;
}

// (number? p)
SchemeBool* s_number_p(SchemeObject* p) {
    return (p->type() == SchemeObject::NUMBER) ? S_TRUE : S_FALSE;
}

// (number? p)
SchemeBool* s_vector_p(SchemeObject* p) {
    return (p->type() == SchemeObject::VECTOR) ? S_TRUE : S_FALSE;
}

SchemeObject* s_car(SchemeObject* o) {
    if (o->type() != SchemeObject::PAIR) {
        throw scheme_exception("Wrong type");
    }
    return static_cast<SchemePair*>(o)->car;
}

SchemeObject* s_cdr(SchemeObject* o) {
    if (o->type() != SchemeObject::PAIR) {
        throw scheme_exception("Wrong type");
    }
    return static_cast<SchemePair*>(o)->cdr;
}

// (cons a b)
SchemePair* s_cons(SchemeObject* car, SchemeObject* cdr) {
    return new SchemePair(car, cdr);
}

SchemePair* s_list(SchemePair* args) {
    return args;
}

SchemeObject* s_display(SchemeObject* o) {
    cout << o->toString();
    return S_UNSPECIFIED;
}

SchemeObject* s_newline(BindingEnvironment* s) {
    cout << endl;        
    return S_UNSPECIFIED;
}

SchemePair* s_reverse(SchemeObject* o) {
    SchemePair* result = S_EMPTY_LIST;
    if (s_list_p(o) != S_TRUE) {
		throw scheme_exception("reverse with wrong argument");
    }
    while (o != S_EMPTY_LIST) {
		SchemePair* l = static_cast<SchemePair*> (o);
		result = s_cons(l->car, result);
		o = l->cdr;
	}
	return result;  
}

SchemeNumber* s_length(SchemePair* p) {
    int length = 0;
    while (p != S_EMPTY_LIST) {
        length++;
        p = p->cdrAsPair();
        if (p == NULL) {
            throw scheme_exception("Not a list");
        }
    }
    return make_number(length);
}

SchemeNumber* s_plus(SchemePair* p) {
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

SchemeNumber* s_minus(SchemePair* p) {
	double result = 0;
    double first = true;
	if (p == S_EMPTY_LIST) {
		return S_ZERO;
	}
	while (p != S_EMPTY_LIST) {
		SchemeNumber* n = static_cast<SchemeNumber*>(p->car);
		if (n == NULL) {
			throw scheme_exception("Wrong argument to +: " + p->car->toString());
		}
		if (first) {
            result = n->number;
            first = false;
		} else {
		    result -= n->number;
	    }
		p = p->cdrAsPair();
	}
	return new SchemeNumber(result);
}


SchemeNumber* s_mult(SchemePair* p) {
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

SchemeVector* s_make_vector(SchemeNumber* count, SchemeObject* obj) {
    int c = int(count->number);
    return new SchemeVector(obj, c);
}

SchemeVector* s_vector(SchemePair* args) {
    int c = int(s_length(args)->number);
    SchemeObject** elems = new SchemeObject*[c];
    int i = 0;
    while (args != S_EMPTY_LIST) {
        elems[i++] = args->car;
        args = args->cdrAsPair();
    }
    return new SchemeVector(elems,c);
}

SchemeNumber* s_vector_length(SchemeObject* v) {
    if (s_vector_p(v) == S_FALSE) {
        throw scheme_exception("Not a vector");
    }
    SchemeVector* vv = static_cast<SchemeVector*>(v);
    return make_number(vv->length);
}

SchemeVector* s_list_2_vector(SchemePair* list) {
    return s_vector(list);
}

SchemePair* s_vector_2_list(SchemeVector* v) {
    SchemePair* result = S_EMPTY_LIST;
    for(int i = v->length-1; i >= 0; i--) {
	result = s_cons(v->get(i), result);
    }
    return result;
}

SchemeObject* s_vector_ref(SchemeVector* v, SchemeNumber* index) {
    int i = int(index->number);
    return v->get(i);
}

SchemeObject* s_vector_set_e(SchemeVector* vec, SchemeNumber* index, SchemeObject* val) {
    int i = int(index->number);
    vec->set(val,i);
    return S_UNSPECIFIED;
}
