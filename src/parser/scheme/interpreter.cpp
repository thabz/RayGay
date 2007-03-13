
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

Interpreter::Interpreter(SchemePair* parsetree, BindingEnvironment* top_level_bindings) {
    this->parsetree = parsetree;   
	this->top_level_bindings = top_level_bindings;
}

SchemeObject* Interpreter::interpret() {
	SchemeObject* result = S_UNSPECIFIED;
	if (parsetree == S_EMPTY_LIST) {
		return result;
	}
    SchemePair* p = static_cast<SchemePair*> (parsetree);
	while (p->type() != SchemeObject::EMPTY_LIST) {
		result = eval(top_level_bindings, p->car);
		p = static_cast<SchemePair*>(p->cdr);
	}
	return result;
}

SchemeObject* Interpreter::eval(BindingEnvironment* envt, SchemeObject* s) {
	SchemeObject::ObjectType type = s->type();
	switch(type) {
		case SchemeObject::SYMBOL:
            return eval_symbol(envt, static_cast<SchemeSymbol*>(s));
		case SchemeObject::NUMBER:
		case SchemeObject::STRING:
		case SchemeObject::BOOL:
		case SchemeObject::EMPTY_LIST:
		    // Selfevaluating type
 		    return s;
        case SchemeObject::PAIR:
		    return eval_list(envt, static_cast<SchemePair*>(s));
 		default:
	  	    throw scheme_exception("Unknown type");
	}
	return NULL;
}

SchemeObject* Interpreter::eval_list(BindingEnvironment* envt, SchemePair* p) {
    SchemeSymbol* s = static_cast<SchemeSymbol*>(p->car);
	SchemePair* cdr = static_cast<SchemePair*>(p->cdr);
	if (s == NULL) {
		throw scheme_exception("Wrong type to apply: " + p->toString());
	}
    if (s->str == "if") {
		return eval_if(envt, cdr);
    } else {
		throw scheme_exception("Unbound variable: " + s->toString());	
    }
	return NULL;
}

SchemeObject* Interpreter::eval_symbol(BindingEnvironment* envt, SchemeSymbol* p) {
    return envt->get(p->str)->obj;
}


SchemeObject* Interpreter::eval_if(BindingEnvironment* envt, SchemePair* p) {
	SchemeObject* s_condition = p->car;
	SchemeObject* true_case = p->cdrAsPair()->car;
	SchemeObject* false_case = p->cdrAsPair()->cdrAsPair()->car;
	
	bool condition = eval(envt, s_condition)->boolValue();
	return condition ? eval(envt, true_case) : eval(envt, false_case);
}
