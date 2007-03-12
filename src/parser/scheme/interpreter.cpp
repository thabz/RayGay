
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

Interpreter::Interpreter(SchemeObject* parsetree, Scheme* scheme) {
    this->parsetree = parsetree;   
	this->scheme = scheme;
}

SchemeObject* Interpreter::interpret() {
	SchemeObject* result;
	if (parsetree == Scheme::S_EMPTY_LIST) {
		return parsetree;
	}
    SchemePair* p = static_cast<SchemePair*> (parsetree);
	while (p->type() != SchemeObject::EMPTY_LIST) {
		result = eval(p->car);
		p = static_cast<SchemePair*>(p->cdr);
	}
	return result;
}

SchemeObject* Interpreter::eval(SchemeObject* s) {
	SchemeObject::ObjectType type = s->type();
	switch(type) {
		case SchemeObject::NUMBER:
		case SchemeObject::STRING:
		case SchemeObject::BOOL:
		case SchemeObject::SYMBOL:
		case SchemeObject::EMPTY_LIST:
		    // Selfevaluating type
 		    return s;
        case SchemeObject::PAIR:
		    return eval_list(static_cast<SchemePair*>(s));
 		default:
	  	    scheme->die_with_error("Unknown type");
	}
	return NULL;
}

SchemeObject* Interpreter::eval_list(SchemePair* p) {
    SchemeSymbol* s = static_cast<SchemeSymbol*>(p->car);
	SchemePair* cdr = static_cast<SchemePair*>(p->cdr);
	if (s == NULL) {
		scheme->die_with_error("Wrong type to apply: " + p->toString());
	}
    if (s->str == "+") {
		return eval_plus(cdr);
    } else if (s->str == "*") {
		return eval_mult(cdr);
    } else if (s->str == "if") {
		return eval_if(cdr);
    } else {
		scheme->die_with_error("Unbound variable: " + s->toString());	
    }
	return NULL;
}


SchemeNumber* Interpreter::eval_plus(SchemePair* p) {
	double result = 0;
	if (p == Scheme::S_EMPTY_LIST) {
		return Scheme::S_ZERO;
	}
	while (p != Scheme::S_EMPTY_LIST) {
		SchemeNumber* n = static_cast<SchemeNumber*>(eval(p->car));
		if (n == NULL) {
			scheme->die_with_error("Wrong argument to +: " + p->car->toString());
		}
		result += n->number;
		p = p->cdrAsPair();
	}
	return new SchemeNumber(result);
}


SchemeNumber* Interpreter::eval_mult(SchemePair* p) {
	double result = 1;
	if (p == Scheme::S_EMPTY_LIST) {
		return Scheme::S_ONE;
	}
	while (p != Scheme::S_EMPTY_LIST) {
		SchemeNumber* n = static_cast<SchemeNumber*>(eval(p->car));
		if (n == NULL) {
			scheme->die_with_error("Wrong argument to *: " + p->car->toString());
		}
		result *= n->number;
		p = p->cdrAsPair();
	}
	return new SchemeNumber(result);
}

SchemeObject* Interpreter::eval_if(SchemePair* p) {
	SchemeObject* s_condition = p->car;
	SchemeObject* true_case = p->cdrAsPair()->car;
	SchemeObject* false_case = p->cdrAsPair()->cdrAsPair()->car;
	
	bool condition = eval(s_condition)->boolValue();
	return condition ? eval(true_case) : eval(false_case);
}
