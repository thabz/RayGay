
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

//------------------------------------------------------------------------
// Stack
//------------------------------------------------------------------------
Stack::Stack() {
}

int Stack::size() {
    return stk.size();
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
    return eval(top_level_bindings, parsetree);
}

Stack* tstack = new Stack();

#define call_and_return(envt,thing,PLACE) { int kk = setjmp(*(tstack->push_jump_pos())); \
    if (kk == 0) { \
        tstack->push(envt); \
        tstack->push(thing); \
        goto PLACE; \
    } \
}

SchemeObject* eval(BindingEnvironment* envt_orig, SchemeObject* seq_orig) {
    
    SchemeSymbol* if_symbol = SchemeSymbol::create("if");
    SchemeSymbol* let_symbol = SchemeSymbol::create("let");
    SchemeSymbol* begin_symbol = SchemeSymbol::create("begin");
    SchemeSymbol* and_symbol = SchemeSymbol::create("and");
    SchemeSymbol* or_symbol = SchemeSymbol::create("or");
    SchemeSymbol* lambda_symbol = SchemeSymbol::create("lambda");
    SchemeSymbol* apply_symbol = SchemeSymbol::create("apply");
    SchemeSymbol* quote_symbol = SchemeSymbol::create("quote");
    SchemeSymbol* quasiquote_symbol = SchemeSymbol::create("quasiquote");
    SchemeSymbol* define_symbol = SchemeSymbol::create("define");
    SchemeSymbol* set_e_symbol = SchemeSymbol::create("set!");
    
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
    		case SchemeObject::VECTOR:
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
    	
        if (s == if_symbol) {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_IF;
    	} else if (s == quote_symbol) {
            tstack->return_jump(cdr->car);
    	} else if (s == define_symbol) {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_DEFINE;
    	} else if (s == quasiquote_symbol) {
            tstack->push(envt);
            tstack->push(cdr->car);
    		goto EVAL_QUASIQUOTE;
    	} else if (s == apply_symbol) {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_APPLY;
    	} else if (s == lambda_symbol) {
    	    SchemeObject* formals = cdr->car;
            SchemeObject* body = cdr->cdr;
            tstack->push(envt);
            tstack->push(formals);
            tstack->push(body);
            goto EVAL_LAMBDA;	
    	} else if (s == let_symbol) {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_LET;
    	} else if (s == set_e_symbol) {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_SET_E;
    	} else if (s == begin_symbol) {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_SEQUENCE;
    	} else if (s == and_symbol) {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_AND;
    	} else if (s == or_symbol) {
            tstack->push(envt);
            tstack->push(cdr);
    		goto EVAL_OR;
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
            
            tstack->push(proc);
            tstack->push(envt);
            int kk = setjmp(*(tstack->push_jump_pos()));
            if (kk == 0) {
                tstack->push(envt);
                tstack->push(cdr);
                goto EVAL_MULTI;
            }
            SchemePair* args = tstack->popSchemePair();
            envt = tstack->popBindingEnvironment();
            proc = static_cast<SchemeProcedure*>(tstack->popSchemeObject());
            
            tstack->push(envt);
            tstack->push(args);
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
        call_and_return(envt,s->car,EVAL);
        SchemeObject* proc = tstack->popSchemeObject();
        s = tstack->popSchemePair(); // Pop local var
        envt = tstack->popBindingEnvironment(); // Pop local var

        SchemePair* arg_expressions = static_cast<SchemePair*>(s->cdr);
        
        if (proc->type() != SchemeObject::PROCEDURE) {
		    throw scheme_exception("Wrong type to apply: " + s->toString() + " does not resolve to a procedure.");
        }
        
        tstack->push(proc);
        tstack->push(envt);
        int kk = setjmp(*(tstack->push_jump_pos()));
        if (kk == 0) {
            tstack->push(envt);
            tstack->push(arg_expressions);
            goto EVAL_MULTI;
        }
        SchemePair* args = tstack->popSchemePair();
        envt = tstack->popBindingEnvironment();
        proc = static_cast<SchemeProcedure*>(tstack->popSchemeObject());
        
        tstack->push(envt);
        tstack->push(args);
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
        call_and_return(envt,s_condition_expr,EVAL);
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
    EVAL_AND: {
        // TODO: Optimize tail-recursion
        SchemePair* p = static_cast<SchemePair*>(tstack->popSchemeObject());
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        
        SchemeObject* result = S_TRUE;
        while (p != S_EMPTY_LIST) {
            if (p->cdr == S_EMPTY_LIST) {
                // Optimize tail-recursion
                tstack->push(envt);
                tstack->push(p->car);
                goto EVAL;
            }
            tstack->push(envt);
            tstack->push(p);
            call_and_return(envt,p->car,EVAL);
            result = tstack->popSchemeObject();
            p = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
            envt = tstack->popBindingEnvironment(); // Pop local var

            if (!result->boolValue()) {
                tstack->return_jump(result);
            }
            p = p->cdrAsPair();
        } 
        tstack->return_jump(result);
    }
    EVAL_OR: {
        SchemePair* p = static_cast<SchemePair*>(tstack->popSchemeObject());
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        
        SchemeObject* result = S_FALSE;
        while (p != S_EMPTY_LIST) {
            if (p->cdr == S_EMPTY_LIST) {
                // Optimize tail-recursion
                tstack->push(envt);
                tstack->push(p->car);
                goto EVAL;
            }
            
            tstack->push(envt);
            tstack->push(p);
            int kk = setjmp(*(tstack->push_jump_pos()));
            if (kk == 0) {
                tstack->push(envt);
                tstack->push(p->car);
                goto EVAL;
            }
            result = tstack->popSchemeObject();
            p = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
            envt = tstack->popBindingEnvironment(); // Pop local var
            if (result->boolValue()) {
                tstack->return_jump(result);
            }
            p = p->cdrAsPair();
        } 
        tstack->return_jump(result);
    }
    EVAL_QUASIQUOTE: {
        SchemeObject* o = tstack->popSchemeObject();
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        if (o->type() == SchemeObject::PAIR) {
            SchemePair* p = static_cast<SchemePair*>(o);
            SchemePair* result = S_EMPTY_LIST;
            while(p != S_EMPTY_LIST) {
                SchemeObject* to_add;
                SchemePair* v = static_cast<SchemePair*>(p->car);
                if (p->car->type() == SchemeObject::PAIR && v->car->type() == SchemePair::SYMBOL) {
                    SchemeSymbol* sy = static_cast<SchemeSymbol*>(v->car);
                    if (sy->str == "unquote") {
                        tstack->push(envt);
                        tstack->push(result);
                        tstack->push(p);
                        int kk = setjmp(*(tstack->push_jump_pos()));
                        if (kk == 0) {
                            tstack->push(envt);
                            tstack->push(v->cdrAsPair()->car);
                            goto EVAL;
                        }
                        to_add = tstack->popSchemeObject();
                        p = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
                        result = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
                        envt = tstack->popBindingEnvironment(); // Pop local var
                        result = s_cons(to_add, result);
                    } else if (sy->str == "unquote-splicing") {
                        /*
                        if (v->cdrAsPair()->car == S_EMPTY_LIST) {
                            throw scheme_exception("unquote-splicing must result in a non-empty list");
                        }
                        */
                        tstack->push(envt);
                        tstack->push(result);
                        tstack->push(p);
                        call_and_return(envt, v->cdrAsPair()->car, EVAL);
                        SchemeObject* to_add_list = tstack->popSchemeObject();
                        p = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
                        result = static_cast<SchemePair*>(tstack->popSchemeObject()); // Pop local var
                        envt = tstack->popBindingEnvironment(); // Pop local var
                        if (s_pair_p(to_add_list) != S_TRUE) {
                            throw scheme_exception("unquote-splicing must result in a non-empty list");
                        }
                        // TODO: Code below is inefficient, but works.
                        to_add_list = s_reverse(static_cast<SchemePair*>(to_add_list));
                        result = static_cast<SchemePair*>(s_append(s_cons(to_add_list, s_cons(result,S_EMPTY_LIST))));
                        p = p->cdrAsPair();
                        continue;
                    } else {
                        result = s_cons(p->car, result);
                    }
                } else {
                    result = s_cons(p->car, result);
                }
                p = p->cdrAsPair();
            }
            result = s_reverse(result);
            tstack->return_jump(result);
        } else {
            tstack->return_jump(o);
        }
    }
    EVAL_PROCEDURE_CALL: {
        SchemeProcedure* proc = static_cast<SchemeProcedure*>(tstack->popSchemeObject());
        SchemePair* args = tstack->popSchemePair();
        BindingEnvironment* envt = tstack->popBindingEnvironment();

        SchemeObject* result = S_UNSPECIFIED;

        /*
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
        */
        if (proc->fn != NULL) {
            SchemeObject* argsv[10];
            // Built-in function
            int args_num = int(s_length(args)->number);
            if (args_num < proc->req) {
                throw scheme_exception("Too few argument given.");
            }
            if (args_num > proc->req + proc->opt && proc->rst == 0) {
                throw scheme_exception("Too many argument given.");
            }
            if (args_num < proc->req + proc->opt) {
 	            // TODO: append_e nogle S_UNSPECIFIED p� args, s� l�ngden af args bliver req+opt.
            }
            SchemePair* a_p = args;
            if (proc->rst == 0) {
                for(int i = 0; i < 10; i++) {
                    if (i < args_num) {
                        argsv[i] = a_p->car;
                        a_p = a_p->cdrAsPair();
                    } else {
                        argsv[i] = S_UNSPECIFIED;
                    }
                }
                switch(proc->req + proc->opt) {
                    case 0:   result = (*((SchemeObject* (*)())(proc->fn)))();
                              break;
                    case 1:   result = (*((SchemeObject* (*)(SchemeObject*))(proc->fn)))(argsv[0]);
                              break;
                    case 2:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1]);
                              break;
                    case 3:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2]);
                              break;
                    default:  throw scheme_exception("Arguments mismatch"); 
                }
            } else {
                for(int i = 0; i < 10; i++) {
                    if (i < proc->req) {
                        argsv[i] = a_p->car;
                        a_p = a_p->cdrAsPair();
                    } else {
                        argsv[i] = a_p;
                        break;
                    }
                }
                switch(proc->req + proc->opt) {
                    case 0:   result = (*((SchemeObject* (*)(SchemeObject*))(proc->fn)))(argsv[0]);
                              break;
                    case 1:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1]);
                              break;
                    case 2:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2]);
                              break;
                    case 3:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3]);
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
            call_and_return(envt, p->car, EVAL);
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
                call_and_return(envt, p->car, EVAL);
                tstack->pop(); // Unused result
                p = tstack->popSchemePair(); // Pop local var
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
            call_and_return(envt, p->cdrAsPair()->car, EVAL);
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
    EVAL_APPLY: {
        // args is a list (proc arg1 arg2 ... argn). argn must be a list. proc is called with the arguments
        // (append (list arg1 arg2 ...) argn)
        SchemePair* args = tstack->popSchemePair();
        BindingEnvironment* envt = tstack->popBindingEnvironment();

        // Eval the procedure argument
        tstack->push(envt);
        tstack->push(args); // Push local var
        call_and_return(envt,args->car,EVAL);
        SchemeObject* proc = tstack->popSchemeObject();
        args = tstack->popSchemePair();             // Pop local var
        envt = tstack->popBindingEnvironment();                

        if (s_procedure_p(proc) == S_FALSE) {
            throw scheme_exception("Can't apply to non-procedure: " + args->car->toString());
        }
        if (s_pair_p(args->cdr) == S_FALSE) {
            throw scheme_exception("Wrong type in position 1.");
        }

        // Eval and join all arg-lists together
        tstack->push(envt);
        call_and_return(envt,args->cdr,EVAL_MULTI);
        args = tstack->popSchemePair();
        envt = tstack->popBindingEnvironment();                
        
        SchemePair* collected = S_EMPTY_LIST;
        SchemePair* prev = NULL;
        while (args != S_EMPTY_LIST) {
            SchemeObject* arg = args->car;
            if (s_pair_p(arg) == S_TRUE || arg == S_EMPTY_LIST) {
                if (args->cdr == S_EMPTY_LIST) {
                    // arg is a list and last argument
                    if (collected == S_EMPTY_LIST) {
                        collected = static_cast<SchemePair*>(arg);
                    } else {
                        prev->cdr = arg;
                    }
                } else {
                    throw scheme_exception("Illegal argument");
                }
            } else {
                if (collected == S_EMPTY_LIST) {
                    collected = s_cons(arg, S_EMPTY_LIST);
                    prev = collected;
                } else {
                    SchemePair* tmp = s_cons(arg,S_EMPTY_LIST);
                    prev->cdr = tmp;
                    prev = tmp;
                }
            }
            args = args->cdrAsPair();
        }
        tstack->push(envt);
        tstack->push(collected);
        tstack->push(proc);
        goto EVAL_PROCEDURE_CALL;
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
        call_and_return(envt, p->cdrAsPair()->car, EVAL);
        SchemeObject* v = tstack->popSchemeObject();
        s = static_cast<SchemeSymbol*>(tstack->popSchemeObject()); // Pop local var
        envt = tstack->popBindingEnvironment();                

        envt->put(s, v);

        //envt->put(s, eval(envt, p->cdrAsPair()->car));
        tstack->return_jump(S_UNSPECIFIED);
    }
    EVAL_LET: {
        SchemePair* p = tstack->popSchemePair();
        BindingEnvironment* envt = tstack->popBindingEnvironment();
        
        if (s_symbol_p(p->car) == S_TRUE) {
            // Named let
            throw scheme_exception("Named let not implemented yet");
        }
        if (s_pair_p(p->car) == S_FALSE && s_null_p(p->car) == S_FALSE) {
            // Named let
            throw scheme_exception("Bad body in let");
        }
        
        // Build new bindings
        BindingEnvironment* new_bindings = new BindingEnvironment(envt);
        SchemePair* binding_pairs = static_cast<SchemePair*>(p->car);

        while (binding_pairs != S_EMPTY_LIST) {
            // Eval binding value
            tstack->push(envt);
            tstack->push(p);
            tstack->push(new_bindings);
            tstack->push(binding_pairs);
            call_and_return(envt,s_car(s_cdr(s_car(binding_pairs))),EVAL);
            SchemeObject* val = tstack->popSchemeObject();
            binding_pairs = tstack->popSchemePair();
            new_bindings = tstack->popBindingEnvironment();
            p = tstack->popSchemePair();
            envt = tstack->popBindingEnvironment();                
            new_bindings->put(static_cast<SchemeSymbol*>(s_car(s_car(binding_pairs))), val);
            binding_pairs = binding_pairs->cdrAsPair();
        }
        
        tstack->push(new_bindings);
        tstack->push(p->cdr); // Push local var
        goto EVAL_SEQUENCE;
    }
    return NULL;
}
