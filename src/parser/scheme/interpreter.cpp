
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
    	SchemeObject* car = p->car;
        
        if (car->type() != SchemeObject::SYMBOL) {
            return eval_combo(envt, p);
        }

        SchemeSymbol* s = static_cast<SchemeSymbol*>(car);
    	SchemePair* cdr = static_cast<SchemePair*>(p->cdr);
    	
        if (s->str == "if") {
    		return eval_if(envt, cdr);
    	} else if (s->str == "let") {
            return eval_let(envt, cdr);	
    	} else if (s->str == "define") {
            return eval_define(envt, cdr);	
    	} else if (s->str == "quote") {
            return eval_quote(envt, cdr);	
        } else {
            SchemeObject* obj = envt->get(s->str);
            if (obj == NULL) {
        		throw scheme_exception("Unbound variable: " + s->toString());	
            }
            if (obj->type() != SchemeObject::PROCEDURE) {
        		throw scheme_exception("Wrong type to apply : " + obj->toString());	
            }
            return eval_procedure_call(envt, static_cast<SchemeProcedure*>(obj), cdr);
        }
    } catch (scheme_exception e) {
        cout << "In expression " << p->toString() << ":" << endl;
        throw e;
    }
	return NULL;
}

SchemeObject* Interpreter::eval_symbol(BindingEnvironment* envt, SchemeSymbol* p) {
    SchemeObject* o = envt->get(p->str);
    if (o == NULL) {
        throw scheme_exception("Unbound variable " + p->str);
    }
    return o;
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

// Evals a list of expressions and returns a list of results
SchemePair* Interpreter::eval_multi(BindingEnvironment* envt, SchemePair* p) {
	SchemePair* result = S_EMPTY_LIST;
	while (p->type() != SchemeObject::EMPTY_LIST) {
		result = s_cons(NULL, eval(envt, p->car), result);
		p = p->cdrAsPair();
	}
	return s_reverse(NULL, result);
}

SchemeObject* Interpreter::eval_combo(BindingEnvironment* envt, SchemePair* s) {
    // (car s) is an expression that should evaluate to a function that we execute
    SchemePair* cdr = static_cast<SchemePair*>(s->cdr);
    SchemeObject* car = eval(envt, s->car);
    if (car->type() != SchemeObject::PROCEDURE) {
		throw scheme_exception("Wrong type to apply: " + s->toString() + " does not resolve to a procedure.");
    }
    return eval_procedure_call(envt, static_cast<SchemeProcedure*>(car), cdr);
}

// Find the function in the envt and execute it
SchemeObject* Interpreter::eval_procedure_call(BindingEnvironment* envt, SchemeProcedure* proc, SchemePair* arg_exprs) {
    SchemeObject* result = S_UNSPECIFIED;
    
    // TODO: Check that number of args given and required match
    SchemePair* args = eval_multi(envt, arg_exprs);

    if (proc->fn != NULL) {
        // Built-in function
        switch(proc->req + proc->opt + proc->rst) {
            case 1:   result = (*((SchemeObject* (*)(BindingEnvironment*,SchemeObject*))(proc->fn)))(envt, args->car);
                      break;
            case 2:   result = (*((SchemeObject* (*)(BindingEnvironment*,SchemeObject*,SchemeObject*))(proc->fn)))(envt, args->car, args->cdrAsPair()->car);
                      break;
            case 3:   result = (*((SchemeObject* (*)(BindingEnvironment*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(envt, args->car, args->cdrAsPair()->car, args->cdrAsPair()->cdrAsPair()->car);
                      break;
            default:  throw scheme_exception("Arguments mismatch"); 
        }
    } else {
        // User function
    }
    return result;
}


//------------------------------------------------------------------------
// Evaluators for special forms
//------------------------------------------------------------------------
SchemeObject* Interpreter::eval_define(BindingEnvironment* envt, SchemePair* p) {
    // Handle the two cases (define var value) and (define (func-name args...) forms...)
    if (s_pair_p(envt, p->car ) == S_TRUE) {
        // (define (func-name args...) forms...)
        throw scheme_exception("Not implemented.");
    } else {
        // (define var value-expr)
        if (s_length(envt, p) != S_TWO) {
            throw scheme_exception("Missing or extra expression");
        }
        SchemeSymbol* s = static_cast<SchemeSymbol*>(p->car);
        if (s == NULL) {
            throw scheme_exception("Bad variable");
        }
        envt->put(s->str, eval(envt, p->cdrAsPair()->car));
    }
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

SchemeObject* Interpreter::eval_quote(BindingEnvironment* envt, SchemePair* p) {
	return p->car;
}

SchemeObject* Interpreter::eval_let(BindingEnvironment* envt, SchemePair* p) {
    BindingEnvironment* new_envt = new BindingEnvironment(envt);
    
    // Extract vars from (car p) into new_envt
    
    // eval body of let
	return eval_sequence(new_envt, p->cdrAsPair());
}
