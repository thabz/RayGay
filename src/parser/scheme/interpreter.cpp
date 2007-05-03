
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

SchemeObject* global_ret;
SchemeObject* global_arg1;
SchemeObject* global_arg2;
SchemeObject* global_arg3;
SchemeEnvironment* global_envt;
list<SchemeObject*> stack;


//------------------------------------------------------------------------
// Interpreter
//------------------------------------------------------------------------
Interpreter::Interpreter(SchemeObject* parsetree, SchemeEnvironment* top_level_bindings) {
    this->parsetree = parsetree;   
	this->top_level_bindings = top_level_bindings;
}

SchemeObject* Interpreter::call_procedure_n(SchemeObject* procedure, SchemeObject* args) {
    global_arg1 = procedure;
    global_arg2 = args;
    stack.push_back(global_arg1);
    stack.push_back(global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call);
    stack.pop_back();
    stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_0(SchemeObject* procedure) {
    global_arg1 = procedure;
    global_arg2 = S_EMPTY_LIST;
    stack.push_back(global_arg1);
    stack.push_back(global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call);
    stack.pop_back();
    stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_1(SchemeObject* procedure, SchemeObject* arg) {
    global_arg1 = procedure;
    global_arg2 = s_cons(arg,S_EMPTY_LIST);
    stack.push_back(global_arg1);
    stack.push_back(global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call);
    stack.pop_back();
    stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_2(SchemeObject* procedure, SchemeObject* arg1, SchemeObject* arg2) {
    global_arg1 = procedure;
    global_arg2 = s_cons(arg1, s_cons(arg2, S_EMPTY_LIST));
    stack.push_back(global_arg1);
    stack.push_back(global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call);
    stack.pop_back();
    stack.pop_back();
    return result;
}

SchemeObject* Interpreter::interpret() {
    if (parsetree == S_EMPTY_LIST) {
	    return S_UNSPECIFIED;
    }
    stack.push_back(parsetree);
    stack.push_back(top_level_bindings);
    global_arg1 = parsetree;
    global_envt = top_level_bindings;
    SchemeObject* result = trampoline((fn_ptr)&eval_sequence);
    stack.pop_back();
    stack.pop_back();
    return result;
}

// A popular method for achieving proper tail recursion in a non-tail-recursive C implementation 
// is a trampoline.[2] A trampoline is an outer function which iteratively calls an inner function. 
// The inner function returns the address of another function to call, and the outer function then 
// calls this new function. In other words, when an inner function wishes to call another inner 
// function tail-recursively, it returns the address of the function it wants to call back to the 
// trampoline, which then calls the returned function. By returning before calling, the stack is 
// first popped so that it does not grow without bound on a simple iteration. Unfortunately, the 
// cost of such a trampoline function call is 2-3 times slower than a normal C call, and it requires 
// that arguments be passed in global variables [Tarditi92].

SchemeObject* trampoline(fn_ptr f) {
    SchemeEnvironment* saved = global_envt;
    size_t stack_size = stack.size();
    stack.push_back(saved);
    while (f != NULL) {
        f = (fn_ptr)(*f)();
    }
    global_envt = saved;
    stack.resize(stack_size);
    return global_ret;
}

fn_ptr eval() {
    SchemeObject* s = global_arg1;
    SchemeEnvironment* envt = global_envt;

    Heap* heap = Heap::getUniqueInstance();
    if (heap->timeToGarbageCollect()) {
        heap->addRoot(s);
        heap->addRoot(envt);
        heap->garbageCollect(stack);
        heap->popRoot();
        heap->popRoot();
    }
    
	SchemeObject::ObjectType type = s->type();
	switch(type) {
		case SchemeObject::SYMBOL: {
            SchemeSymbol* symbol = static_cast<SchemeSymbol*>(s);
		    s = envt->get(symbol);
            if (s == NULL) {
                throw scheme_exception("Unbound variable " + symbol->str);
            }
            global_ret = s;
            return NULL;
	    }
		case SchemeObject::NUMBER:
		case SchemeObject::STRING:
		case SchemeObject::BOOL:
		case SchemeObject::CHAR:
		case SchemeObject::VECTOR:
		case SchemeObject::EMPTY_LIST:
		case SchemeObject::INPUT_PORT:
		case SchemeObject::OUTPUT_PORT:
            global_ret = s;
            return NULL;
        case SchemeObject::PAIR:
            global_arg1 = s;
            return (fn_ptr)&eval_list;
 		default:
	  	    throw scheme_exception("Unknown type: " + s->toString());
	}
}

fn_ptr eval_list() {
    SchemeObject* p = global_arg1;
    SchemeEnvironment* envt = global_envt;
    
	SchemeObject* car = s_car(p);
    if (s_symbol_p(car) == S_FALSE) {
        return (fn_ptr)&eval_combo;
    }

    SchemeSymbol* s = static_cast<SchemeSymbol*>(car);
	SchemeObject* cdr = s_cdr(p);
	
	SchemeObject* proc = envt->get(s);
    if (proc != NULL) {
        if (proc->type() == SchemeObject::MACRO) {
            global_arg1 = proc;
            global_arg2 = cdr;
            return (fn_ptr)&eval_call_macro;
        } else if (proc->type() == SchemeObject::PROCEDURE) {
            stack.push_back(proc);
            stack.push_back(cdr);
            global_arg1 = cdr;
            SchemeObject* args = trampoline((fn_ptr)&eval_multi);
            stack.pop_back();
            stack.pop_back();
            global_arg1 = proc;
            global_arg2 = args;
            return (fn_ptr)&eval_procedure_call;
        } else if (proc->type() == SchemeObject::CONTINUATION) {
            global_arg1 = s_car(cdr);
            eval();
            static_cast<SchemeContinuation*>(proc)->call(global_ret);
        } else if (proc->type() == SchemeObject::INTERNAL_PROCEDURE) {
            if (s == if_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_if;
        	} else if (s == quote_symbol) {
                global_ret = s_car(cdr);
                return NULL;
        	} else if (s == define_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_define;
        	} else if (s == define_macro) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_define_macro;
        	} else if (s == quasiquote_symbol) {
                global_arg1 = s_car(cdr);
                return (fn_ptr)&eval_quasiquote;
        	} else if (s == lambda_symbol) {
        	    SchemeObject* formals = s_car(cdr);
                SchemeObject* body = s_cdr(cdr);
                global_arg1 = formals;
                global_arg2 = body;
                global_arg3 = unnamed_symbol;
                return (fn_ptr)&eval_lambda;
        	} else if (s == let_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_let;
        	} else if (s == do_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_do;
        	} else if (s == letstar_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_letstar;
        	} else if (s == letrec_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_letrec;
        	} else if (s == apply_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_apply;
        	} else if (s == set_e_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_set_e;
        	} else if (s == begin_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_begin;
        	} else if (s == cond_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_cond;
        	} else if (s == case_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_case;
        	} else if (s == and_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_and;
        	} else if (s == or_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_or;
            } else {
                throw scheme_exception("Unknown internal procedure: " + proc->toString());	
            }            
        } else {
            throw scheme_exception("Wrong type to apply : " + proc->toString());	
        }
    } else {
		throw scheme_exception("Unbound variable: " + s->toString());	
    }
}

//
// Evals a list of expressions and returns the last.
//
fn_ptr eval_sequence() {
    SchemeObject* p = global_arg1;
    if (p == S_EMPTY_LIST) {
        global_ret = S_UNSPECIFIED;
        return NULL;
    }
    stack.push_back(p);
    stack.push_back(global_envt);
    while (true) {
        if (s_null_p(s_cdr(p)) == S_TRUE) {
            // The tail call, let EVAL return to this' caller
            stack.pop_back();
            stack.pop_back();
            global_arg1 = s_car(p);
            return (fn_ptr)&eval;
        } else {
            global_arg1 = s_car(p);
            trampoline((fn_ptr)&eval);
            p = s_cdr(p);
        }
    }    
}

//
// Evals a list of expressions and the returns the list of results
//
fn_ptr eval_multi() {
    // TODO: Facilitate allocation of multiple cells at once instead
    // of separate calls to s_cons.
    SchemeObject* p = global_arg1;
    stack.push_back(p);
    stack.push_back(global_envt);
    
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* tail_pair = S_EMPTY_LIST;
    stack.push_back(result);
    SchemeObject** result_stack_ref = &(stack.back());
    
    while (p != S_EMPTY_LIST) {
        global_arg1 = s_car(p);
        SchemeObject* r = trampoline((fn_ptr)&eval);

	    if (result == S_EMPTY_LIST) {
	        result = s_cons(r, S_EMPTY_LIST);
            tail_pair = result;
            *result_stack_ref = result;
	    } else {
	        SchemeObject* new_tail = s_cons(r, S_EMPTY_LIST);
	        s_set_cdr_e(tail_pair, new_tail);
	        tail_pair = new_tail;
	    }

        p = s_cdr(p);
    }
    stack.pop_back();
    stack.pop_back();
    stack.pop_back();
    global_ret = result;
    return NULL;
}


fn_ptr eval_define() {
    SchemeObject* p = global_arg1;
    SchemeEnvironment* envt = global_envt;
    
    if (s_pair_p(s_car(p)) == S_TRUE) {
        // (define (func-name args...) body-forms...)
        SchemeObject* pa = s_car(p);
        if (s_symbol_p(s_car(pa)) == S_FALSE) {
            throw scheme_exception("Bad variable");
        }
        SchemeObject* body = s_cdr(p);
        SchemeObject* name = s_car(pa);

        global_arg1 = s_cdr(pa);
        global_arg2 = body;
        global_arg3 = name;
        trampoline((fn_ptr)&eval_lambda);
        SchemeObject* proc = global_ret;
        
        envt->put(static_cast<SchemeSymbol*>(name), proc);
    } else {
        // (define var value-expr)
        if (s_length(p) != S_TWO) {
            throw scheme_exception("Missing or extra expression");
        }
        
        if (s_symbol_p(s_car(p)) == S_FALSE) {
            throw scheme_exception("Bad variable");
        }
        SchemeSymbol* s = static_cast<SchemeSymbol*>(s_car(p));
        
        global_arg1 = s_car(s_cdr(p));
        SchemeObject* v = trampoline((fn_ptr)&eval);

        envt->put(s, v);
    }
    global_ret = S_UNSPECIFIED;
    return NULL;
}


fn_ptr eval_begin() {
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_apply() {
    SchemeObject* p = global_arg1;
    stack.push_back(p);
    
    if (s_procedure_p(s_car(p)) == S_TRUE) {
        stack.pop_back();
        global_arg1 = s_car(p);
        global_arg2 = s_car(s_cdr(p));
        return (fn_ptr)&eval_procedure_call;
    }
    
    global_arg1 = s_car(p);
    SchemeObject* proc = trampoline((fn_ptr)&eval);
    assert_arg_type("apply", 1, s_procedure_p, proc);
    
    global_arg1 = s_cdr(p);
    SchemeObject* args = trampoline((fn_ptr)&eval_multi);

    SchemePair* collected = S_EMPTY_LIST;
    SchemePair* prev = NULL;
    int i = 0;
    while (args != S_EMPTY_LIST) {
        i++;
        SchemeObject* arg = s_car(args);
        if (s_pair_p(arg) == S_TRUE || arg == S_EMPTY_LIST) {
            if (s_cdr(args) == S_EMPTY_LIST) {
                // arg is a list and last argument
                if (collected == S_EMPTY_LIST) {
                    collected = static_cast<SchemePair*>(arg);
                } else {
                    s_set_cdr_e(prev, arg);
                }
            } else {
                throw scheme_exception("Illegal argument");
            }
        } else {
            if (collected == S_EMPTY_LIST) {
                collected = s_cons(arg, S_EMPTY_LIST);
                prev = collected;
                stack.push_back(collected);
            } else {
                SchemePair* tmp = s_cons(arg,S_EMPTY_LIST);
                s_set_cdr_e(prev, tmp);
                prev = tmp;
            }
        }
        args = s_cdr(args);
    }

    if (proc->type() == SchemeObject::INTERNAL_PROCEDURE) {
        if (collected != S_EMPTY_LIST) {
            stack.pop_back();
        }
        stack.pop_back();
        // Hack to handle the test (apply apply `(,+ ,(list 1 2)))
        global_arg1 = s_cons(s_car(p), collected);
        return (fn_ptr)&eval_list;
    }

    if (collected != S_EMPTY_LIST) {
        stack.pop_back();
    }
    stack.pop_back();
    global_arg1 = proc;
    global_arg2 = collected;
    return (fn_ptr)&eval_procedure_call;
}

//
// ((form) args)		    
// where form is an expression that should evaluate to a function that we execute
//
fn_ptr eval_combo() {
    SchemeObject* s = global_arg1;
    stack.push_back(s);
    
    global_arg1 = s_car(s);
    SchemeObject* proc = trampoline((fn_ptr)&eval);
    
    if (proc->type() != SchemeObject::PROCEDURE) {
	    throw scheme_exception("Wrong type to apply: " + s->toString() + " does not resolve to a procedure.");
    }
    
    stack.push_back(proc);
    
    global_arg1 = s_cdr(s);
    SchemeObject* args = trampoline((fn_ptr)&eval_multi);
    
    stack.pop_back();
    stack.pop_back();
    
    global_arg1 = proc;
    global_arg2 = args;
    return (fn_ptr)&eval_procedure_call;
}

//
// (if condition true-form false-form) 
// where false-form is optional.
//
fn_ptr eval_if() {
    SchemeObject* p = global_arg1;

    stack.push_back(p);
    global_arg1 = s_car(p);
    bool condition = trampoline((fn_ptr)&eval)->boolValue();
    stack.pop_back();
	
    if (condition) {
        // Evaluate and return true case
        SchemeObject* true_case = s_car(s_cdr(p));
        global_arg1 = true_case;
        return (fn_ptr)&eval;
    } else if (s_cdr(s_cdr(p)) != S_EMPTY_LIST) {
        // Evaluate and return false case
    	SchemeObject* false_case = s_car(s_cdr(s_cdr(p)));
        global_arg1 = false_case;
        return (fn_ptr)&eval;
    } else {
        global_ret = S_UNSPECIFIED;
        return NULL;
    }    
}

fn_ptr eval_and() {
    SchemeObject* p = global_arg1;
    stack.push_back(p);
    
    SchemeObject* result = S_TRUE;
    while (p != S_EMPTY_LIST) {
        if (s_cdr(p) == S_EMPTY_LIST) {
            // Tail call
            stack.pop_back();
            global_arg1 = s_car(p);
            return (fn_ptr)&eval;
        } else {
            global_arg1 = s_car(p);
            result = trampoline((fn_ptr)&eval);

            if (!result->boolValue()) {
                stack.pop_back();
                global_ret = result;
                return NULL;
            }
            p = s_cdr(p);
        } 
    }
    stack.pop_back();
    global_ret = result;
    return NULL;
}

fn_ptr eval_or() {
    SchemeObject* p = global_arg1;
    stack.push_back(p);
    stack.push_back(global_envt);
    
    SchemeObject* result = S_FALSE;
    while (p != S_EMPTY_LIST) {
        if (s_cdr(p) == S_EMPTY_LIST) {
            // Tail call
            stack.pop_back();
            stack.pop_back();
            global_arg1 = s_car(p);
            return (fn_ptr)&eval;
        } else {
            global_arg1 = s_car(p);
            result = trampoline((fn_ptr)&eval);

            if (result->boolValue()) {
                stack.pop_back();
                stack.pop_back();
                global_ret = result;
                return NULL;
            }
            p = s_cdr(p);
        }
    }
    stack.pop_back();
    stack.pop_back();
    global_ret = result;
    return NULL;
}

SchemeObject* eval_quasiquote_recursive(SchemeObject* o, int level);

SchemeObject* eval_unquote_recursive(SchemeObject* o, int level) {
    SchemeObject* result;
    stack.push_back(o);
    if (level == 0) {
        global_arg1 = s_car(s_cdr(o));
        result = trampoline((fn_ptr)&eval);
    } else {
        result = s_cons(unquote_symbol, eval_quasiquote_recursive(s_cdr(o),level-1));
    }
    stack.pop_back();
    return result;
}

SchemeObject* eval_quasiquote_recursive(SchemeObject* o, int level) {
    //cout << "Level " << level << ": " << o->toString() << endl;
    SchemeObject* p = o;
    if (s_vector_p(o) == S_TRUE) {
        p = s_vector_2_list(o);
    } else {
        p = o;
    }

    stack.push_back(S_EMPTY_LIST);
    SchemeObject*& result = stack.back();

    if (s_pair_p(p) == S_TRUE) {
        SchemeObject* car_p = s_car(p);
        if (car_p == unquote_symbol && s_vector_p(o) == S_FALSE) {
            result = eval_unquote_recursive(p,level);
        } else if (car_p == unquote_splicing_symbol && s_vector_p(o) == S_FALSE) {
            result = eval_unquote_recursive(p,level);
            if (s_list_p(result) == S_FALSE) {
                throw scheme_exception("unquote-splicing must result in a list");
            }
        } else if (car_p == quasiquote_symbol && level == 0) {
            result = s_cons(quasiquote_symbol, eval_quasiquote_recursive(s_cdr(o), level + 1));
        } else {
            // p is a list or vector that we recurse into
            result = S_EMPTY_LIST;
            SchemeObject* prev = S_EMPTY_LIST;
            while(true) {
                if (s_pair_p(p) == S_TRUE) {
                    
                    if (s_car(p) == unquote_symbol) {
                        // Handle when final cdr of unproper list is a unquote
                        s_set_cdr_e(prev,eval_unquote_recursive(p, level));
                        break;
                    }
                    
                    stack.push_back(p);
                    SchemeObject* r = eval_quasiquote_recursive(s_car(p),level);
                    stack.pop_back();

                    if (s_pair_p(s_car(p)) == S_TRUE && s_car(s_car(p)) == unquote_splicing_symbol) {
                        // Splice into result
                        if (result == S_EMPTY_LIST) {
                            result = r;
                            prev = result;
                        } else {
                            s_set_cdr_e(prev,r);
                        }
                        // Forward-wind prev to point to end of newly added list
                        while(s_null_p(s_cdr(prev)) == S_FALSE) {
                            prev = s_cdr(prev);
                        } 
                    } else {
                        if (result == S_EMPTY_LIST) {
                            result = s_cons(r, S_EMPTY_LIST);
                            prev = result;
                        } else {
                            SchemeObject* tmp = s_cons(r, S_EMPTY_LIST);
                            s_set_cdr_e(prev,tmp);
                            prev = tmp;
                        }
                        
                    }
                    p = s_cdr(p);
                } else if (p == S_EMPTY_LIST) {
                    break;
                } else {
                    SchemeObject* r = eval_quasiquote_recursive(p,level);
                    if (result != S_EMPTY_LIST) {
                        s_set_cdr_e(prev,r);
                    } else {
                        result = r;  
                    }
                    break;
                }
            }
        }

        stack.pop_back();
        if (s_vector_p(o) == S_TRUE) {
            return s_list_2_vector(result);
        } else {
            return result;
        }
    } else {
        stack.pop_back();
        return o;
    }
}

fn_ptr eval_quasiquote() {
    global_ret = eval_quasiquote_recursive(global_arg1,0);
    return NULL;
}

fn_ptr eval_procedure_call() {
    SchemeProcedure* proc = static_cast<SchemeProcedure*>(global_arg1);
    SchemeObject* args = global_arg2;
    
    stack.push_back(proc);
    stack.push_back(args);

    SchemeObject* result = S_UNSPECIFIED;

    if (proc->fn != NULL) {
        SchemeObject* argsv[10];
        // Built-in function
        int args_num = int(s_length(args)->number);
        if (args_num < proc->req) {
            throw scheme_exception("Too few argument given in call to "+proc->nameAsString());
        }
        if (args_num > proc->req + proc->opt && proc->rst == 0) {
            throw scheme_exception("Too many argument given in call to "+proc->nameAsString());
        }
        if (args_num < proc->req + proc->opt) {
            // TODO: append_e nogle S_UNSPECIFIED på args, så længden af args bliver req+opt.
        }
        SchemeObject* a_p = args;
        try {
            if (proc->rst == 0) {
                for(int i = 0; i < 10; i++) {
                    if (i < args_num) {
                        argsv[i] = s_car(a_p);
                        a_p = s_cdr(a_p);
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
                        argsv[i] = s_car(a_p);
                        a_p = s_cdr(a_p);
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
        } catch (scheme_exception e) {
            string s = "In call to " + proc->nameAsString() + ": " + e.str;
            throw scheme_exception(s);
        }
        stack.pop_back();
        stack.pop_back();
        global_ret = result;
        return NULL;
    } else {
        // User function
        SchemeEnvironment* new_envt = SchemeEnvironment::create(proc->envt);
        SchemeObject* req_symbols = proc->s_req;
        while (req_symbols != S_EMPTY_LIST) {
            if (args == S_EMPTY_LIST) {
                throw scheme_exception("Too few argument given in call to "+proc->nameAsString());
            }
            new_envt->put(s_car(req_symbols), s_car(args));
            req_symbols = s_cdr(req_symbols);
            args = s_cdr(args);
        }
        if (proc->rst == 0 && args != S_EMPTY_LIST) {
            throw scheme_exception("Too many argument given in call to "+proc->nameAsString());
        }
        if (proc->rst == 1) {
            new_envt->put(proc->s_rst, args);
        }
        global_envt = new_envt;
        global_arg1 = proc->s_body;
        stack.pop_back();
        stack.pop_back();
        return (fn_ptr)&eval_sequence;
    }    
}

// TODO: Don't split the formals into req and rst, but simply
// store it directly into procedure.
// eval_procedure_call then traverses the original formals form
// when doing the bindings.
// This results in a much fast eval_lambda without any cons'ing.
// and eval_procedure_call won't suffer much speedwise if at all.
fn_ptr eval_lambda() {
    SchemeObject* formals = global_arg1;
    SchemeObject* body = global_arg2;
    SchemeObject* name = global_arg3;
    SchemeEnvironment* envt = global_envt;

    SchemeSymbol* rst;
    SchemePair* req = S_EMPTY_LIST;
    SchemeObject* req_tail = S_EMPTY_LIST;
    if (s_symbol_p(formals) == S_TRUE) {
        rst = static_cast<SchemeSymbol*>(formals);
    } else if (s_pair_p(formals) == S_TRUE) {
        while (s_pair_p(formals) == S_TRUE) {
            SchemePair* pp = static_cast<SchemePair*>(formals);
            if (s_symbol_p(s_car(pp)) == S_FALSE) {
                throw scheme_exception("Bad formals");                
            }
	    if (req == S_EMPTY_LIST) {
		req = s_cons(s_car(pp), S_EMPTY_LIST);
		req_tail = req;
	    } else {
		SchemeObject* tmp = s_cons(s_car(pp), S_EMPTY_LIST);
		s_set_cdr_e(req_tail, tmp);
		req_tail = tmp;
	    }
            formals = s_cdr(pp);
        }

        if (formals != S_EMPTY_LIST) {
	        // Handle the rest argument
            if (s_symbol_p(formals) == S_FALSE) {
                throw scheme_exception("Bad formals");                
            }
            rst = static_cast<SchemeSymbol*>(formals);
        } else {
            rst = NULL;
        }
    } else if (formals == S_EMPTY_LIST) {
        rst = NULL;
    } else {
        throw scheme_exception("Bad formals");
    }

    global_ret = SchemeProcedure::create(name, envt, req, rst, body);
    return NULL;
}

fn_ptr eval_set_e() {
    // TODO: We're doing double hashtable lookups. Both a envt->get(s) and in envt->set(s,v). Optimize by letting envt->get(s) return a binding, that
    // we manipulate instead of doing the set(s,v)

    SchemeObject* p = global_arg1;
    SchemeEnvironment* envt = global_envt;

    if (s_length(p) != S_TWO) {
        throw scheme_exception("Missing or extra expression");
    }
    SchemeObject* car = s_car(p);
    if (s_symbol_p(car) == S_FALSE) {
        throw scheme_exception("Wrong type argument in position 1.");
    }
    SchemeSymbol* s = static_cast<SchemeSymbol*>(car);

    SchemeObject* already_bound = envt->get(s);
    if (already_bound == NULL) {
        throw scheme_exception("Unbound variable: " + s->toString());
    }
    
    global_arg1 = s_car(s_cdr(p));
    SchemeObject* v = trampoline((fn_ptr)&eval);

    envt->set(s, v);

    global_ret = S_UNSPECIFIED;
    return NULL;
}

fn_ptr eval_cond() {
    SchemeObject* p = global_arg1;

    while (s_null_p(p) == S_FALSE) {
        SchemeObject* clause = s_car(p);
        if (s_pair_p(clause) == S_FALSE) {
            throw scheme_exception("Invalid clause");
        }
        SchemeObject* test_expr = s_car(clause);
        if (test_expr == else_symbol) {
            // Handle (else <expressions> ...)
            if (s_null_p(s_cdr(p)) == S_FALSE) {
                throw scheme_exception("else-clause must be last");
            }
            global_arg1 = s_cdr(clause);
            return (fn_ptr)&eval_sequence;
        }
        
        // Eval test_expr
        global_arg1 = test_expr;
        SchemeObject* test = trampoline((fn_ptr)&eval);
        
        if (test->boolValue()) {
            if (s_cdr(clause) == S_EMPTY_LIST) {
                global_ret = S_UNSPECIFIED;
                return NULL;
            } else if (s_car(s_cdr(clause)) == ergo_symbol) {
                // Handle (<test> => <expression>)
                global_arg1 = s_car(s_cdr(s_cdr(clause)));
                SchemeObject* proc = trampoline((fn_ptr)&eval);

                global_arg1 = proc;
                global_arg2 = s_cons(test,S_EMPTY_LIST);
                return (fn_ptr)&eval_procedure_call;
            } else {
                global_arg1 = s_cdr(clause);
                return (fn_ptr)&eval_sequence;
            }
        }
        p = s_cdr(p);
    }
    global_ret = S_UNSPECIFIED;
    return NULL;
}

fn_ptr eval_case() {
    SchemeObject* p = global_arg1;
    stack.push_back(p);

    // Eval key       
    global_arg1 = s_car(p);
    SchemeObject* key = trampoline((fn_ptr)&eval);
    
    p = s_cdr(p);

    while (s_null_p(p) == S_FALSE) {
        SchemeObject* clause = s_car(p);
        if (s_pair_p(clause) == S_FALSE) {
            throw scheme_exception("Invalid clause");
        }
        SchemeObject* clause_car = s_car(clause);
        if (clause_car == else_symbol) {
            // Handle (else <expressions> ...)
            if (s_null_p(s_cdr(p)) == S_FALSE) {
                throw scheme_exception("else-clause must be last");
            }
            stack.pop_back();
            global_arg1 = s_cdr(clause);
            return (fn_ptr)&eval_sequence;
        } else if (s_pair_p(clause_car) == S_TRUE) {
            if (s_memv(key,clause_car) != S_FALSE) {
                stack.pop_back();
                global_arg1 = s_cdr(clause);
                return (fn_ptr)&eval_sequence;
            }
        } else {
            throw scheme_exception("Invalid clause in case-statement");
        }

        p = s_cdr(p);
    }
    stack.pop_back();
    global_ret = S_UNSPECIFIED;
    return NULL;    
}

fn_ptr eval_let() {
    SchemeObject* p = global_arg1;
    SchemeEnvironment* envt = global_envt;

    if (s_symbol_p(s_car(p)) == S_TRUE) {
        return (fn_ptr)&eval_named_let;
    }
    if (s_pair_p(s_car(p)) == S_FALSE && s_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad body in let");
    }
    
    // Build new bindings
    SchemeEnvironment* new_bindings = SchemeEnvironment::create(envt);
    SchemeObject* binding_pairs = s_car(p);
    
    stack.push_back(new_bindings);

    while (s_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        
        global_arg1 = s_car(s_cdr(s_car(binding_pairs)));
        SchemeObject* val = trampoline((fn_ptr)&eval);

        new_bindings->put(static_cast<SchemeSymbol*>(s_car(s_car(binding_pairs))), val);
        binding_pairs = s_cdr(binding_pairs);
    }
    stack.pop_back();
    
    global_envt = new_bindings;
    global_arg1 = s_cdr(p);
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_named_let() {
    SchemeObject* p = global_arg1;
    SchemeEnvironment* envt = global_envt;
    
    stack.push_back(p);
    stack.push_back(envt);

    SchemeObject* name = s_car(p);
    p = s_cdr(p);
    
    if (s_pair_p(s_car(p)) == S_FALSE && s_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad formals in let");
    }
    
    // Extract formals and collect evaluated args for a lambda
    SchemeObject* formals = S_EMPTY_LIST;
    SchemeObject* formals_tail = S_EMPTY_LIST;
    stack.push_back(formals);
    SchemeObject*& formals_ref = stack.back();
    SchemeObject* args = S_EMPTY_LIST;
    SchemeObject* args_tail = S_EMPTY_LIST;
    stack.push_back(args);
    SchemeObject*& args_ref = stack.back();
    SchemeObject* binding_pairs = s_car(p);

    while (s_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        global_arg1 = s_car(s_cdr(s_car(binding_pairs)));
        SchemeObject* val = trampoline((fn_ptr)&eval);
        
	if (formals == S_EMPTY_LIST) {
	    formals = s_cons(s_car(s_car(binding_pairs)), S_EMPTY_LIST);
	    formals_tail = formals;
	    formals_ref = formals;
	} else {
	    SchemeObject* tmp = s_cons(s_car(s_car(binding_pairs)), S_EMPTY_LIST);
	    s_set_cdr_e(formals_tail,tmp);
	    formals_tail = tmp;
	}

	if (args == S_EMPTY_LIST) {
	    args = s_cons(val, S_EMPTY_LIST);
	    args_tail = args;
	    args_ref = args;
	} else {
	    SchemeObject* tmp = s_cons(val, S_EMPTY_LIST);
	    s_set_cdr_e(args_tail, tmp);
	    args_tail = tmp;

	}

        binding_pairs = s_cdr(binding_pairs);
    }
    
    stack.pop_back();
    stack.pop_back();
    stack.pop_back();
    stack.pop_back();

    SchemeEnvironment* new_envt = SchemeEnvironment::create(envt);
    SchemeProcedure* lambda = SchemeProcedure::create(name, new_envt, formals, NULL, static_cast<SchemePair*>(s_cdr(p)));
    new_envt->put(static_cast<SchemeSymbol*>(name), lambda);
    
    global_arg1 = lambda;
    global_arg2 = args;
    global_envt = new_envt;
    return (fn_ptr)&eval_procedure_call;
}

fn_ptr eval_letstar() {
    SchemeObject* p = global_arg1;
    SchemeEnvironment* envt = global_envt;
    
    if (s_pair_p(s_car(p)) == S_FALSE && s_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad formals in let*: " + s_car(p)->toString());
    }
    
    if (s_null_p(s_cdr(p)) == S_TRUE) {
        throw scheme_exception("Missing body in let*");
    }

    // Build new bindings
    SchemeEnvironment* new_bindings = SchemeEnvironment::create(envt);
    SchemeObject* binding_pairs = s_car(p);

    while (s_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        global_arg1 = s_cadar(binding_pairs);
        global_envt = new_bindings;
        SchemeObject* val = trampoline((fn_ptr)&eval);
        
	SchemeSymbol* sym = static_cast<SchemeSymbol*>(s_car(s_car(binding_pairs)));
	if (sym == NULL) {
	    throw scheme_exception("Bad variable in let*: " + s_car(s_car(binding_pairs))->toString());
	}
        new_bindings->put(sym, val);
        binding_pairs = s_cdr(binding_pairs);
    }
    
    global_envt = new_bindings;
    global_arg1 = s_cdr(p);
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_letrec() {
    return (fn_ptr)&eval_letstar;
}

fn_ptr eval_define_macro() {
    SchemeObject* p = global_arg1;
    SchemeEnvironment* envt = global_envt;

    SchemeObject* formals = s_car(p);
    SchemePair* body = static_cast<SchemePair*>(s_cdr(p));
    SchemeObject* name = s_car(formals);
    formals = s_cdr(formals);
    
    if (s_symbol_p(name) == S_FALSE) {
        throw scheme_exception("Invalid macro-name in definition");
    }
    
    SchemeSymbol* rst;
    SchemePair* req = S_EMPTY_LIST;
    SchemeObject* req_tail = S_EMPTY_LIST;
    if (s_pair_p(formals) == S_TRUE) {
        while (s_pair_p(formals) == S_TRUE) {
            SchemePair* pp = static_cast<SchemePair*>(formals);
            if (s_symbol_p(s_car(pp)) == S_FALSE) {
                throw scheme_exception("Bad formals");                
            }
	    if (req == S_EMPTY_LIST) {
		req = s_cons(s_car(pp), S_EMPTY_LIST);
		req_tail = req;
	    } else {
		SchemeObject* tmp = s_cons(s_car(pp), S_EMPTY_LIST);
		s_set_cdr_e(req_tail, tmp);
		req_tail = tmp;
	    }
            formals = s_cdr(pp);
        }
        if (formals != S_EMPTY_LIST) {
            if (s_symbol_p(formals) == S_FALSE) {
                throw scheme_exception("Bad formals");                
            }
            rst = static_cast<SchemeSymbol*>(formals);
        } else {
            rst = NULL;
        }
    } else if (formals == S_EMPTY_LIST) {
        rst = NULL;
    } else {
        throw scheme_exception("Bad formals");
    }
    
    SchemeMacro* macro = SchemeMacro::create(name, envt, req, rst, body);
    envt->put(static_cast<SchemeSymbol*>(name), macro);
    
    global_ret = S_UNSPECIFIED;
    return (fn_ptr)NULL;
}

fn_ptr eval_call_macro() {
    SchemeMacro* proc = static_cast<SchemeMacro*>(global_arg1);
    SchemeObject* args = global_arg2;
    SchemeEnvironment* envt = global_envt;
    
    // Build new environment
    SchemeEnvironment* new_envt = SchemeEnvironment::create(proc->envt);
    SchemeObject* req_symbols = proc->s_req;
    while (req_symbols != S_EMPTY_LIST) {
        if (args == S_EMPTY_LIST) {
            throw scheme_exception("Too few argument given.");
        }
        new_envt->put(s_car(req_symbols), s_car(args));
        req_symbols = s_cdr(req_symbols);
        args = s_cdr(args);
    }
    if (proc->rst == 0 && args != S_EMPTY_LIST) {
        throw scheme_exception("Too many argument given.");
    }
    if (proc->rst == 1) {
        new_envt->put(proc->s_rst, args);
    }
    // cout << "Body: " << proc->s_body->toString() << endl;
    // Transform body
    stack.push_back(envt);
    stack.push_back(proc);
    stack.push_back(new_envt);
    global_envt = new_envt;
    global_arg1 = proc->s_body;
    SchemeObject* transformed_body = trampoline((fn_ptr)&eval_sequence);
    //cout << "Transformed body: " << transformed_body->toString() << endl;
    stack.pop_back();
    stack.pop_back();
    stack.pop_back();
    
    // Eval transformed body
    global_envt = envt;
    global_arg1 = transformed_body;
    return (fn_ptr)&eval;    
}

fn_ptr eval_do() {
    SchemeObject* p = global_arg1;
    SchemeEnvironment* envt = global_envt;

    if (s_pair_p(s_car(p)) == S_FALSE && s_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad body in let");
    }

    // Extract formals and collect evaluated args for a lambda
    SchemeEnvironment* new_envt = SchemeEnvironment::create(envt);
    SchemeObject* steps = S_EMPTY_LIST;
    SchemeObject* varnames = S_EMPTY_LIST;
    SchemeObject* binding_pairs = s_car(p);
    
    stack.push_back(p);
    stack.push_back(new_envt);
    stack.push_back(steps);
    SchemeObject*& steps_stack_pos = stack.back();
    stack.push_back(varnames);
    SchemeObject*& varnames_stack_pos = stack.back();

    while (s_null_p(binding_pairs) == S_FALSE) {
        // Eval initial binding value
        global_arg1 = s_car(s_cdr(s_car(binding_pairs)));
        SchemeObject* val = trampoline((fn_ptr)&eval);
        
        SchemeObject* varname = s_car(s_car(binding_pairs));
        if (s_symbol_p(varname) == S_FALSE) {
            throw scheme_exception("Invalid variable in do: " + varname->toString());
        }
        new_envt->put(static_cast<SchemeSymbol*>(varname), val);
        varnames = s_cons(varname, varnames);
        varnames_stack_pos = varnames;

        SchemeObject* step = s_cdr(s_cdr(s_car(binding_pairs)));
        if (step == S_EMPTY_LIST) {
            step = varname;
        } else {
            step = s_car(step);
        }
        steps = s_cons(step, steps);
        steps_stack_pos = steps;

        binding_pairs = s_cdr(binding_pairs);
    }
    
    SchemeObject* body = s_cdr(p);
    
    global_envt = new_envt;
    
    while (true) {
        // Evaluate test
        global_arg1 = s_car(s_car(body));
        SchemeObject* val = trampoline((fn_ptr)&eval);

        // Return if test is true
        if (val->boolValue()) {
            if (s_cdr(s_car(body)) == S_EMPTY_LIST) {
                global_ret = S_UNSPECIFIED;
                stack.pop_back();
                stack.pop_back();
                stack.pop_back();
                stack.pop_back();
                return NULL;
            } else {
                global_arg1 = s_cdr(s_car(body));
                stack.pop_back();
                stack.pop_back();
                stack.pop_back();
                stack.pop_back();
                return (fn_ptr)&eval_sequence;
            }
        }
        
        if (s_cdr(body) != S_EMPTY_LIST) {
            global_arg1 = s_cdr(body);
            trampoline((fn_ptr)&eval_sequence);
        }
        
        // Evaluate steps
        global_arg1 = steps;
        SchemeObject* vals = trampoline((fn_ptr)&eval_multi);
        
        // Assign new step values
        SchemeObject* tmp = varnames;
        while(s_null_p(varnames) == S_FALSE) {
            new_envt->put(static_cast<SchemeSymbol*>(s_car(varnames)), s_car(vals));
            varnames = s_cdr(varnames);
            vals = s_cdr(vals);
        }
        varnames = tmp;
    }
    return NULL; /* Never reached */
}
