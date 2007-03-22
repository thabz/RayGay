
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

//------------------------------------------------------------------------
// Stack
//------------------------------------------------------------------------
Stack::Stack() {
    
}

SchemeObject* Stack::popSchemeObject() {
    assert(!stk.empty());
    assert(stk.back().type == StackEntry::OBJECT);
    SchemeObject* result = stk.back().s_object;
    stk.pop_back();
    return result;
}

SchemePair* Stack::popSchemePair() {
    assert(!stk.empty());
    assert(stk.back().type == StackEntry::OBJECT);
    SchemeObject* result = stk.back().s_object;
    assert(result->type() == SchemeObject::PAIR || result->type() == SchemeObject::EMPTY_LIST);
    stk.pop_back();
    return static_cast<SchemePair*>(result);
}


BindingEnvironment* Stack::popBindingEnvironment() {
    assert(!stk.empty());
    assert(stk.back().type == StackEntry::ENVT);
    BindingEnvironment* result = stk.back().envt;
    stk.pop_back();
    return result;
}

void Stack::push(SchemeObject* o) {
    StackEntry e;
    e.s_object = o;
    e.type = StackEntry::OBJECT;
    stk.push_back(e);
}

void Stack::push(BindingEnvironment* envt) {
    StackEntry e;
    e.envt = envt;
    e.type = StackEntry::ENVT;
    stk.push_back(e);
}


void Stack::return_jump(SchemeObject* return_value) {
    assert(!stk.empty());
    assert(stk.back().type == StackEntry::JMP_BUF);
    StackEntry e = stk.back();
    stk.pop_back();
    push(return_value);
    longjmp(e.jmpbuf,1);
}

void Stack::pop() {
    assert(!stk.empty());
    stk.pop_back();
}

jmp_buf* Stack::push_jump_pos() {
    StackEntry e;
    e.type = StackEntry::JMP_BUF;
    stk.push_back(e);
    return &(stk.back().jmpbuf);
}

//------------------------------------------------------------------------
// Interpreter
//------------------------------------------------------------------------
Interpreter::Interpreter(SchemePair* parsetree, BindingEnvironment* top_level_bindings) {
    this->parsetree = parsetree;   
	this->top_level_bindings = top_level_bindings;
}

SchemeObject* Interpreter::interpret() {
    return eval2(top_level_bindings, parsetree);
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
    	} else if (s->str == "set!") {
            return eval_set_e(envt, cdr);	
    	} else if (s->str == "quote") {
            return eval_quote(envt, cdr);	
    	} else if (s->str == "begin") {
            return eval_sequence(envt, cdr);	
    	} else if (s->str == "lambda") {
    	    SchemeObject* formals = cdr->car;
            SchemePair* body = cdr->cdrAsPair();
            return eval_lambda(envt, formals, body);	
        } else {
            SchemeObject* obj = envt->get(s);
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
    SchemeObject* o = envt->get(p);
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
		result = s_cons(eval(envt, p->car), result);
		p = p->cdrAsPair();
	}
	return s_reverse(result);
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
    
    SchemePair* args = eval_multi(envt, arg_exprs);

    if (proc->fn != NULL) {
        // Built-in function
        // TODO: Check that number of args given and required do match
        if (proc->rst == 0) {
            switch(proc->req + proc->opt) {
                case 0:   result = (*((SchemeObject* (*)())(proc->fn)))();
                          break;
                case 1:   result = (*((SchemeObject* (*)(SchemeObject*))(proc->fn)))(args->car);
                          break;
                case 2:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdrAsPair()->car);
                          break;
                case 3:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdrAsPair()->car, args->cdrAsPair()->cdrAsPair()->car);
                          break;
                default:  throw scheme_exception("Arguments mismatch"); 
            }
        } else {
            switch(proc->req + proc->opt) {
                case 0:   result = (*((SchemeObject* (*)(SchemeObject*))(proc->fn)))(args);
                          break;
                case 1:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdr);
                          break;
                case 2:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdrAsPair()->car, args->cdrAsPair()->cdr);
                          break;
                case 3:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdrAsPair()->car, args->cdrAsPair()->cdrAsPair()->car, args->cdrAsPair()->cdrAsPair()->cdr);
                          break;
                default:  throw scheme_exception("Arguments mismatch"); 
            }
        }
    } else {
        // User function
        BindingEnvironment* new_envt = new BindingEnvironment(proc->envt);
        SchemePair* req_symbols = proc->s_req;
        while (req_symbols != S_EMPTY_LIST) {
            if (args == S_EMPTY_LIST) {
                throw scheme_exception("Too few argument given.");
            }
            new_envt->put(static_cast<SchemeSymbol*>(req_symbols->car), args->car);
            req_symbols = req_symbols->cdrAsPair();
            args = args->cdrAsPair();
        }
        if (args != S_EMPTY_LIST) {
            if (proc->rst == 0) {
                throw scheme_exception("Too many argument given.");
            } else {
                new_envt->put(proc->s_rst, args);
            }
        }
        result = eval_sequence(new_envt, proc->s_body);
        delete new_envt;
    }
    return result;
}


//------------------------------------------------------------------------
// Evaluators for special forms
//------------------------------------------------------------------------
SchemeObject* Interpreter::eval_define(BindingEnvironment* envt, SchemePair* p) {
    if (s_pair_p(p->car ) == S_TRUE) {
        // (define (func-name args...) body-forms...)
        SchemePair* pa = static_cast<SchemePair*>(p->car);
        if (pa->car->type() != SchemeObject::SYMBOL) {
            throw scheme_exception("Bad variable");
        }
        SchemePair* body = p->cdrAsPair();
        SchemeProcedure* proc = eval_lambda(envt, pa->cdr, body);
        envt->put(static_cast<SchemeSymbol*>(pa->car), proc);
    } else {
        // (define var value-expr)
        if (s_length(p) != S_TWO) {
            throw scheme_exception("Missing or extra expression");
        }
        SchemeSymbol* s = static_cast<SchemeSymbol*>(p->car);
        if (s == NULL) {
            throw scheme_exception("Bad variable");
        }
        envt->put(s, eval(envt, p->cdrAsPair()->car));
    }
    return S_UNSPECIFIED;
}

SchemeObject* Interpreter::eval_set_e(BindingEnvironment* envt, SchemePair* p) {
    if (s_length(p) != S_TWO) {
        throw scheme_exception("Missing or extra expression");
    }
    SchemeObject* car = p->car;
    if (car->type() != SchemeObject::SYMBOL) {
        throw scheme_exception("Wrong type argument in position 1.");
    }
    SchemeSymbol* s = static_cast<SchemeSymbol*>(car);
    
    SchemeObject* already_bound = envt->get(s);
    if (already_bound == NULL) {
        throw scheme_exception("Unbound variable: " + s->toString());
    }
    // TODO: Reuse bound object if same type
    envt->put(s, eval(envt, p->cdrAsPair()->car));
    return S_UNSPECIFIED;
}

SchemeObject* Interpreter::eval_if(BindingEnvironment* envt, SchemePair* p) {
	SchemeObject* s_condition = p->car;
	SchemeObject* true_case = p->cdrAsPair()->car;
	SchemeObject* false_case = p->cdrAsPair()->cdrAsPair()->car;
	
	bool condition = eval(envt, s_condition)->boolValue();
	return condition ? eval(envt, true_case) : eval(envt, false_case);
}

SchemeProcedure* Interpreter::eval_lambda(BindingEnvironment* envt, SchemeObject* formals, SchemePair* body) {
    SchemeSymbol* rst;
    SchemePair* req;
    if (s_symbol_p(formals) == S_TRUE) {
        rst = static_cast<SchemeSymbol*>(formals);
        req = S_EMPTY_LIST;
    } else if (s_pair_p(formals) == S_TRUE) {
        req = S_EMPTY_LIST;
        while (s_pair_p(formals) == S_TRUE) {
            SchemePair* pp = static_cast<SchemePair*>(formals);
            if (s_symbol_p(pp->car) == S_FALSE) {
                throw scheme_exception("Bad formals");                
            }
            req = s_cons(pp->car, req);
            formals = pp->cdr;
        }
        req = s_reverse(req);
        if (formals != S_EMPTY_LIST) {
            if (s_symbol_p(formals) == S_FALSE) {
                throw scheme_exception("Bad formals");                
            }
            rst = static_cast<SchemeSymbol*>(formals);
        } else {
            rst = NULL;
        }
    } else if (formals == S_EMPTY_LIST) {
        req = S_EMPTY_LIST;
        rst = NULL;
    } else {
        throw scheme_exception("Bad formals");
    }
    return new SchemeProcedure(envt, req, rst, body);
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

Stack* tstack = new Stack();

SchemeObject* eval2(BindingEnvironment* envt_orig, SchemeObject* seq_orig) {
    int kk = setjmp(*(tstack->push_jump_pos()));
    if (kk == 0) {
        tstack->push(envt_orig);
        tstack->push(seq_orig);
        goto EVAL_SEQUENCE;
    }
    return tstack->popSchemeObject();
    
    EVAL: {
        SchemeObject* s = tstack->popSchemeObject();
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        
    	SchemeObject::ObjectType type = s->type();
    	switch(type) {
    		case SchemeObject::SYMBOL: {
                SchemeSymbol* symbol = static_cast<SchemeSymbol*>(s);
    		    s = envt->get(symbol);
                if (s == NULL) {
                    throw scheme_exception("Unbound variable " + symbol->str);
                }
                tstack->return_jump(s);
		}
    		case SchemeObject::NUMBER:
    		case SchemeObject::STRING:
    		case SchemeObject::BOOL:
    		case SchemeObject::EMPTY_LIST:
                tstack->return_jump(s);
            case SchemeObject::PAIR:
                tstack->push(envt);
                tstack->push(s);
                goto EVAL_LIST;
     		default:
    	  	    throw scheme_exception("Unknown type");
    	}
    }
    EVAL_LIST: {
        SchemePair* p = tstack->popSchemePair();
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        
    	SchemeObject* car = p->car;
        if (car->type() != SchemeObject::SYMBOL) {
            tstack->push(envt);
            tstack->push(p);
            goto EVAL_COMBO;
        }

        SchemeSymbol* s = static_cast<SchemeSymbol*>(car);
    	SchemePair* cdr = static_cast<SchemePair*>(p->cdr);
    	
        if (s->str == "if") {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_IF;
    	} else if (s->str == "quote") {
            tstack->return_jump(cdr->car);
    	} else if (s->str == "define") {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_DEFINE;
    	} else if (s->str == "lambda") {
    	    SchemeObject* formals = cdr->car;
            SchemeObject* body = cdr->cdr;
            tstack->push(envt);
            tstack->push(formals);
            tstack->push(body);
            goto EVAL_LAMBDA;	
    	/*	
    	} else if (s->str == "let") {
            return eval_let(envt, cdr);	
        */
    	} else if (s->str == "set!") {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_SET_E;
    	} else if (s->str == "begin") {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_SEQUENCE;
        } else {
            // TODO: eval s instead. A symbol evals to  the stored procedure and if this is a combo it also evals to a procedure.
            // Then we can avoid the EVAL_COMBO-handler. But maybe this setup is faster, as EVAL_COMBO isn't used as much, and we
            // then avoid a simple EVAL of a symbol in the majority of cases.
            SchemeObject* proc = envt->get(s);
            if (proc == NULL) {
        		throw scheme_exception("Unbound variable: " + s->toString());	
            }
            if (proc->type() != SchemeObject::PROCEDURE) {
        		throw scheme_exception("Wrong type to apply : " + proc->toString());	
            }
            tstack->push(envt);
            tstack->push(cdr);
            tstack->push(proc);
            goto EVAL_PROCEDURE_CALL;
        }        
    }
    EVAL_COMBO: {
	// ((form) args)		    
	// where form is an expression that should evaluate to a function that we execute
        SchemePair* s = tstack->popSchemePair();
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        
        tstack->push(envt);  // Push local
        tstack->push(s);     // Push local
        int kk = setjmp(*(tstack->push_jump_pos()));
        if (kk == 0) {
            tstack->push(envt);
            tstack->push(s->car);
            goto EVAL;
        }
        SchemeObject* proc = tstack->popSchemeObject();
        s = tstack->popSchemePair(); // Pop local var
        envt = tstack->popBindingEnvironment(); // Pop local var

        SchemePair* arg_expressions = static_cast<SchemePair*>(s->cdr);
        
        if (proc->type() != SchemeObject::PROCEDURE) {
		    throw scheme_exception("Wrong type to apply: " + s->toString() + " does not resolve to a procedure.");
        }
        
        tstack->push(envt);
        tstack->push(arg_expressions);
        tstack->push(proc);
        goto EVAL_PROCEDURE_CALL;
    }
    EVAL_IF: {
	// (if condition true-form false-form) 
	// where false-form is optional.
        SchemePair* p = static_cast<SchemePair*>(tstack->popSchemeObject());
        BindingEnvironment* envt = tstack->popBindingEnvironment();
 
    	SchemeObject* s_condition_expr = p->car;

        tstack->push(envt);
        tstack->push(p);
        int kk = setjmp(*(tstack->push_jump_pos()));
        if (kk == 0) {
            tstack->push(envt);
            tstack->push(s_condition_expr);
            goto EVAL;
        }
    	bool condition = tstack->popSchemeObject()->boolValue();
        p = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
        envt = tstack->popBindingEnvironment(); // Pop local var
    	
    	if (condition) {
    	    // Evaluate and return true case
        	SchemeObject* true_case = p->cdrAsPair()->car;
            tstack->push(envt);
            tstack->push(true_case);
            goto EVAL;
    	} else if (p->cdrAsPair()->cdr != S_EMPTY_LIST) {
    	    // Evaluate and return false case
        	SchemeObject* false_case = p->cdrAsPair()->cdrAsPair()->car;
            tstack->push(envt);
            tstack->push(false_case);
            goto EVAL;
    	} else {
    	    // No false case, return S_UNDEFINED
            tstack->return_jump(S_UNSPECIFIED);
    	}
    }
    EVAL_PROCEDURE_CALL: {
        SchemeProcedure* proc = static_cast<SchemeProcedure*>(tstack->popSchemeObject());
        SchemeObject* arg_exprs = tstack->popSchemeObject();
        BindingEnvironment* envt = tstack->popBindingEnvironment();

        SchemeObject* result = S_UNSPECIFIED;

        tstack->push(proc);
        tstack->push(envt);
        int kk = setjmp(*(tstack->push_jump_pos()));
        if (kk == 0) {
            tstack->push(envt);
            tstack->push(arg_exprs);
            goto EVAL_MULTI;
        }
        SchemePair* args = tstack->popSchemePair();
        envt = tstack->popBindingEnvironment();
        proc = static_cast<SchemeProcedure*>(tstack->popSchemeObject());

        if (proc->fn != NULL) {
            // Built-in function
            // TODO: Check that number of args given and required do match
            if (proc->rst == 0) {
                switch(proc->req + proc->opt) {
                    case 0:   result = (*((SchemeObject* (*)())(proc->fn)))();
                              break;
                    case 1:   result = (*((SchemeObject* (*)(SchemeObject*))(proc->fn)))(args->car);
                              break;
                    case 2:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdrAsPair()->car);
                              break;
                    case 3:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdrAsPair()->car, args->cdrAsPair()->cdrAsPair()->car);
                              break;
                    default:  throw scheme_exception("Arguments mismatch"); 
                }
            } else {
                switch(proc->req + proc->opt) {
                    case 0:   result = (*((SchemeObject* (*)(SchemeObject*))(proc->fn)))(args);
                              break;
                    case 1:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdr);
                              break;
                    case 2:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdrAsPair()->car, args->cdrAsPair()->cdr);
                              break;
                    case 3:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(args->car, args->cdrAsPair()->car, args->cdrAsPair()->cdrAsPair()->car, args->cdrAsPair()->cdrAsPair()->cdr);
                              break;
                    default:  throw scheme_exception("Arguments mismatch"); 
                }
            }
            tstack->return_jump(result);
        } else {
            // User function
            BindingEnvironment* new_envt = new BindingEnvironment(proc->envt);
            SchemePair* req_symbols = proc->s_req;
            while (req_symbols != S_EMPTY_LIST) {
                if (args == S_EMPTY_LIST) {
                    throw scheme_exception("Too few argument given.");
                }
                new_envt->put(static_cast<SchemeSymbol*>(req_symbols->car), args->car);
                req_symbols = req_symbols->cdrAsPair();
                args = args->cdrAsPair();
            }
            if (args != S_EMPTY_LIST) {
                if (proc->rst == 0) {
                    throw scheme_exception("Too many argument given.");
                } else {
                    new_envt->put(proc->s_rst, args);
                }
            }
            // TODO: We're leaking new_envt below
            tstack->push(new_envt);
            tstack->push(proc->s_body);
            goto EVAL_SEQUENCE;
        }
    }
    EVAL_MULTI: {
        // Evals a list of expressions and returns a list of results
        SchemePair* p = tstack->popSchemePair();
        BindingEnvironment* envt = tstack->popBindingEnvironment();

	    SchemePair* result = S_EMPTY_LIST;
	    while (p != S_EMPTY_LIST) {
            tstack->push(envt);     // Push local
            tstack->push(result);   // Push local var
            tstack->push(p);        // Push local var
            int kk = setjmp(*(tstack->push_jump_pos()));
            if (kk == 0) {
                tstack->push(envt);
                tstack->push(p->car);
                goto EVAL;
            }
            SchemeObject* r = tstack->popSchemeObject();
            p = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
            result = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
            envt = tstack->popBindingEnvironment();  // Pop local

		    result = s_cons(r, result);
		    p = p->cdrAsPair();
	    }
        result = s_reverse(result);
        tstack->return_jump(result);
    }
    EVAL_SEQUENCE: {
        // Evals a list of expressions and returns the last.
        SchemePair* p = static_cast<SchemePair*>(tstack->popSchemeObject());
        BindingEnvironment* envt = tstack->popBindingEnvironment();
    	while (true) {
            if (p->cdr == S_EMPTY_LIST) {
                // The tail call, let EVAL return to this' caller
                tstack->push(envt);
                tstack->push(p->car);
                goto EVAL;
            } else {
                // Normal EVAL call, that returns here.
                tstack->push(envt);
                tstack->push(p); // Push local var
                int kk = setjmp(*(tstack->push_jump_pos()));
                if (kk == 0) {
                    tstack->push(envt);
                    tstack->push(p->car);
                    goto EVAL;
                }
                tstack->pop(); // Unused result
                p = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
                envt = tstack->popBindingEnvironment();                
	            p = p->cdrAsPair();
            }
    	}
    }
    EVAL_DEFINE: {
        SchemePair* p = tstack->popSchemePair();
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        if (s_pair_p(p->car ) == S_TRUE) {
            // (define (func-name args...) body-forms...)
            SchemePair* pa = static_cast<SchemePair*>(p->car);
            if (pa->car->type() != SchemeObject::SYMBOL) {
                throw scheme_exception("Bad variable");
            }
            SchemePair* body = p->cdrAsPair();

            tstack->push(envt);
            tstack->push(pa); // Push local var
            int kk = setjmp(*(tstack->push_jump_pos()));
            if (kk == 0) {
                tstack->push(envt);
                tstack->push(pa->cdr);
                tstack->push(body);
                goto EVAL_LAMBDA;
            }
            SchemeObject* proc = tstack->popSchemeObject();
            pa = tstack->popSchemePair();             // Pop local var
            envt = tstack->popBindingEnvironment();                

            //SchemeProcedure* proc = eval_lambda(envt, pa->cdr, body);

            envt->put(static_cast<SchemeSymbol*>(pa->car), proc);
        } else {
            // (define var value-expr)
            if (s_length(p) != S_TWO) {
                throw scheme_exception("Missing or extra expression");
            }
            
            if (p->car->type() != SchemeObject::SYMBOL) {
                throw scheme_exception("Bad variable");
            }
            SchemeSymbol* s = static_cast<SchemeSymbol*>(p->car);
            
            tstack->push(envt);
            tstack->push(s); // Push local var
            int kk = setjmp(*(tstack->push_jump_pos()));
            if (kk == 0) {
                tstack->push(envt);
                tstack->push(p->cdrAsPair()->car);
                goto EVAL;
            }
            SchemeObject* v = tstack->popSchemeObject();
            s = static_cast<SchemeSymbol*>(tstack->popSchemeObject()); // Pop local var
            envt = tstack->popBindingEnvironment();                

            envt->put(s, v);
        }
        tstack->return_jump(S_UNSPECIFIED);
    }
    EVAL_LAMBDA: {
        SchemePair* body = tstack->popSchemePair();
        SchemeObject* formals = tstack->popSchemeObject();
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        
        SchemeSymbol* rst;
        SchemePair* req;
        if (s_symbol_p(formals) == S_TRUE) {
            rst = static_cast<SchemeSymbol*>(formals);
            req = S_EMPTY_LIST;
        } else if (s_pair_p(formals) == S_TRUE) {
            req = S_EMPTY_LIST;
            while (s_pair_p(formals) == S_TRUE) {
                SchemePair* pp = static_cast<SchemePair*>(formals);
                if (s_symbol_p(pp->car) == S_FALSE) {
                    throw scheme_exception("Bad formals");                
                }
                req = s_cons(pp->car, req);
                formals = pp->cdr;
            }
            req = s_reverse(req);
            if (formals != S_EMPTY_LIST) {
                if (s_symbol_p(formals) == S_FALSE) {
                    throw scheme_exception("Bad formals");                
                }
                rst = static_cast<SchemeSymbol*>(formals);
            } else {
                rst = NULL;
            }
        } else if (formals == S_EMPTY_LIST) {
            req = S_EMPTY_LIST;
            rst = NULL;
        } else {
            throw scheme_exception("Bad formals");
        }
        tstack->return_jump(new SchemeProcedure(envt, req, rst, body));
    }
    EVAL_SET_E: {
        SchemePair* p = tstack->popSchemePair();
        BindingEnvironment* envt = tstack->popBindingEnvironment();

        if (s_length(p) != S_TWO) {
            throw scheme_exception("Missing or extra expression");
        }
        SchemeObject* car = p->car;
        if (car->type() != SchemeObject::SYMBOL) {
            throw scheme_exception("Wrong type argument in position 1.");
        }
        SchemeSymbol* s = static_cast<SchemeSymbol*>(car);

        SchemeObject* already_bound = envt->get(s);
        if (already_bound == NULL) {
            throw scheme_exception("Unbound variable: " + s->toString());
        }
        // TODO: Reuse bound object if same type

        tstack->push(envt);
        tstack->push(s); // Push local var
        int kk = setjmp(*(tstack->push_jump_pos()));
        if (kk == 0) {
            tstack->push(envt);
            tstack->push(p->cdrAsPair()->car);
            goto EVAL;
        }
        SchemeObject* v = tstack->popSchemeObject();
        s = static_cast<SchemeSymbol*>(tstack->popSchemeObject()); // Pop local var
        envt = tstack->popBindingEnvironment();                

        envt->put(s, v);

        //envt->put(s, eval(envt, p->cdrAsPair()->car));
        tstack->return_jump(S_UNSPECIFIED);
    }
    return NULL;
}
