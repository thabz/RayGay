
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

Interpreter::Interpreter(SchemePair* parsetree, BindingEnvironment* top_level_bindings) {
    this->parsetree = parsetree;   
	this->top_level_bindings = top_level_bindings;
}

SchemeObject* Interpreter::interpret() {
    return eval_sequence(top_level_bindings, parsetree);
}

SchemeObject* Interpreter::eval(BindingEnvironment* envt, SchemeObject* s) {
    try {
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
	} catch (scheme_exception e) {
        cout << "In expression " << s->toString() << ":" << endl;
        throw e;
	}
	return NULL;
}

SchemeObject* Interpreter::eval_list(BindingEnvironment* envt, SchemePair* p) {
    try {
        SchemeSymbol* s = static_cast<SchemeSymbol*>(p->car);
    	SchemePair* cdr = static_cast<SchemePair*>(p->cdr);
    	if (s == NULL) {
    		throw scheme_exception("Wrong type to apply: " + p->toString());
    	}
        if (s->str == "if") {
    		return eval_if(envt, cdr);
    	} else if (s->str == "let") {
            return eval_let(envt, cdr);	
    	} else if (s->str == "define") {
            return eval_define(envt, cdr);	
        } else {
    		throw scheme_exception("Unbound variable: " + s->toString());	
        }
    } catch (scheme_exception e) {
        cout << "In expression " << p->toString() << ":" << endl;
        throw e;
    }
	return NULL;
}

SchemeObject* Interpreter::eval_symbol(BindingEnvironment* envt, SchemeSymbol* p) {
    Binding* binding = envt->get(p->str);
    if (binding != NULL) {
        return binding->obj;
    } else {
        throw scheme_exception("Unbound variable " + p->str);
    }
}

// Evals a list of expressions and returns the last.
SchemeObject* Interpreter::eval_sequence(BindingEnvironment* envt, SchemePair* p) {
	SchemeObject* result = S_UNSPECIFIED;
	while (p->type() != SchemeObject::EMPTY_LIST) {
		result = eval(envt, p->car);
		p = p->cdrAsPair();
	}
	return result;
}

//------------------------------------------------------------------------
// Evaluators for special forms
//------------------------------------------------------------------------
SchemeObject* Interpreter::eval_define(BindingEnvironment* envt, SchemePair* p) {
    if (s_length(envt, p) != S_TWO) {
        throw scheme_exception("Missing or extra expression");
    }
    SchemeSymbol* s = static_cast<SchemeSymbol*>(p->car);
    if (s == NULL) {
        throw scheme_exception("Bad variable");
    }
    envt->put(s->str, p->cdrAsPair()->car);
    return S_UNSPECIFIED;
}

SchemeObject* Interpreter::eval_set_e(BindingEnvironment* envt, SchemePair* p) {
    // TODO: Reuse bound object if same type
    if (s_length(envt, p) != S_TWO) {
        throw scheme_exception("Missing or extra expression");
    }
    SchemeSymbol* s = static_cast<SchemeSymbol*>(p->car);
    if (s == NULL) {
        throw scheme_exception("Bad variable");
    }
    envt->put(s->str, p->cdrAsPair()->car);
    return S_UNSPECIFIED;
}

SchemeObject* Interpreter::eval_if(BindingEnvironment* envt, SchemePair* p) {
	SchemeObject* s_condition = p->car;
	SchemeObject* true_case = p->cdrAsPair()->car;
	SchemeObject* false_case = p->cdrAsPair()->cdrAsPair()->car;
	
	bool condition = eval(envt, s_condition)->boolValue();
	return condition ? eval(envt, true_case) : eval(envt, false_case);
}

SchemeObject* Interpreter::eval_let(BindingEnvironment* envt, SchemePair* p) {
    BindingEnvironment* new_envt = new BindingEnvironment(envt);
    
    // Extract vars from (car p) into new_envt
    
    // eval body of let
	return eval_sequence(new_envt, p->cdrAsPair());
}
