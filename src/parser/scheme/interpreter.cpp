
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

SchemeSymbol* if_symbol;
SchemeSymbol* cond_symbol;
SchemeSymbol* else_symbol;
SchemeSymbol* ergo_symbol;
SchemeSymbol* case_symbol;
SchemeSymbol* do_symbol;
SchemeSymbol* let_symbol;
SchemeSymbol* letstar_symbol;
SchemeSymbol* letrec_symbol;
SchemeSymbol* begin_symbol;
SchemeSymbol* and_symbol;
SchemeSymbol* or_symbol;
SchemeSymbol* map_symbol;
SchemeSymbol* for_each_symbol;
SchemeSymbol* lambda_symbol;
SchemeSymbol* apply_symbol;
SchemeSymbol* quote_symbol;
SchemeSymbol* quasiquote_symbol;
SchemeSymbol* unquote_symbol;
SchemeSymbol* unquote_splicing_symbol;
SchemeSymbol* define_symbol;
SchemeSymbol* define_macro;
SchemeSymbol* set_e_symbol;

void define_scheme_symbols() {
   if_symbol = SchemeSymbol::create("if");
   cond_symbol = SchemeSymbol::create("cond");
   else_symbol = SchemeSymbol::create("else");
   ergo_symbol = SchemeSymbol::create("=>");
   case_symbol = SchemeSymbol::create("case");
   do_symbol = SchemeSymbol::create("do");
   let_symbol = SchemeSymbol::create("let");
   letstar_symbol = SchemeSymbol::create("let*");
   letrec_symbol = SchemeSymbol::create("letrec");
   begin_symbol = SchemeSymbol::create("begin");
   and_symbol = SchemeSymbol::create("and");
   or_symbol = SchemeSymbol::create("or");
   map_symbol = SchemeSymbol::create("map");
   for_each_symbol = SchemeSymbol::create("for-each");
   lambda_symbol = SchemeSymbol::create("lambda");
   apply_symbol = SchemeSymbol::create("apply");
   quote_symbol = SchemeSymbol::create("quote");
   quasiquote_symbol = SchemeSymbol::create("quasiquote");
   unquote_symbol = SchemeSymbol::create("unquote");
   unquote_splicing_symbol = SchemeSymbol::create("unquote-splicing");
   define_symbol = SchemeSymbol::create("define");
   define_macro = SchemeSymbol::create("define-macro");
   set_e_symbol = SchemeSymbol::create("set!");
}

SchemeObject* global_ret;
SchemeObject* global_arg1;
SchemeObject* global_arg2;
BindingEnvironment* global_envt;


/*
#define call_and_return(envt,thing,PLACE) { int kk = setjmp(*(tstack->push_jump_pos())); \
    if (kk == 0) { \
        tstack->push(envt); \
        tstack->push(thing); \
        goto PLACE; \
    } \
}

void Stack::return_jump(SchemeObject* return_value) {
    assert(!stk.empty());
    assert(stk.back().type == StackEntry::JMP_BUF);
    StackEntry e = stk.back();
    stk.pop_back();
    push(return_value);
    longjmp(e.jmpbuf,1);
}

jmp_buf* Stack::push_jump_pos() {
    StackEntry e;
    e.type = StackEntry::JMP_BUF;
    stk.push_back(e);
    return &(stk.back().jmpbuf);
}
*/

//------------------------------------------------------------------------
// Interpreter
//------------------------------------------------------------------------
Interpreter::Interpreter(SchemePair* parsetree, BindingEnvironment* top_level_bindings) {
    define_scheme_symbols();
    this->parsetree = parsetree;   
	this->top_level_bindings = top_level_bindings;
}

SchemeObject* Interpreter::interpret() {
    if (parsetree == S_EMPTY_LIST) {
	return S_UNSPECIFIED;
    }
    global_arg1 = parsetree;
    global_envt = top_level_bindings;
    return trampoline((fn_ptr)&eval_sequence);
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
    BindingEnvironment* saved = global_envt;
    while (f != NULL) {
        f = (fn_ptr)(*f)();
    }
    global_envt = saved;
    return global_ret;
}

//
// Evals a list of expressions and returns the last.
//
fn_ptr eval_sequence() {
    SchemeObject* p = global_arg1;
    while (true) {
        if (s_null_p(s_cdr(p)) == S_TRUE) {
            // The tail call, let EVAL return to this' caller
            global_arg1 = s_car(p);
            return (fn_ptr)&eval;
        } else {
            global_arg1 = s_car(p);
            trampoline((fn_ptr)&eval);
            p = s_cdr(p);
        }
    }    
}

fn_ptr eval_define() {
    SchemeObject* p = global_arg1;
    BindingEnvironment* envt = global_envt;
    
    if (s_pair_p(s_car(p)) == S_TRUE) {
        // (define (func-name args...) body-forms...)
        SchemeObject* pa = s_car(p);
        if (s_symbol_p(s_car(pa)) == S_FALSE) {
            throw scheme_exception("Bad variable");
        }
        SchemeObject* body = s_cdr(p);

        global_arg1 = s_cdr(pa);
        global_arg2 = body;
        trampoline((fn_ptr)&eval_lambda);
        SchemeObject* proc = global_ret;

        envt->put(static_cast<SchemeSymbol*>(s_car(pa)), proc);
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

fn_ptr eval() {
    SchemeObject* s = global_arg1;
    BindingEnvironment* envt = global_envt;
    
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
    BindingEnvironment* envt = global_envt;
    
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
            global_arg1 = cdr;
            trampoline((fn_ptr)&eval_multi);
            SchemeObject* args = global_ret;

            global_arg1 = proc;
            global_arg2 = args;
            return (fn_ptr)&eval_procedure_call;
        } else {
            throw scheme_exception("Wrong type to apply : " + proc->toString());	
        }
    }
	
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
	} else if (s == apply_symbol) {
        global_arg1 = cdr;
        return (fn_ptr)&eval_apply;
	} else if (s == lambda_symbol) {
	    SchemeObject* formals = s_car(cdr);
        SchemeObject* body = s_cdr(cdr);
        global_arg1 = formals;
        global_arg2 = body;
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
	} else if (s == map_symbol) {
        global_arg1 = cdr;
        return (fn_ptr)&eval_map;
	} else if (s == for_each_symbol) {
        global_arg1 = cdr;
        return (fn_ptr)&eval_for_each;
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
		throw scheme_exception("Unbound variable: " + s->toString());	
    }    
}

fn_ptr eval_begin() {
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_combo() {
    // ((form) args)		    
    // where form is an expression that should evaluate to a function that we execute
    SchemeObject* s = global_arg1;
    
    global_arg1 = s_car(s);
    SchemeObject* proc = trampoline((fn_ptr)&eval);
    
    if (proc->type() != SchemeObject::PROCEDURE) {
	    throw scheme_exception("Wrong type to apply: " + s->toString() + " does not resolve to a procedure.");
    }
    
    global_arg1 = s_cdr(s);
    SchemeObject* args = trampoline((fn_ptr)&eval_multi);
    
    global_arg1 = proc;
    global_arg2 = args;
    return (fn_ptr)&eval_procedure_call;
}

fn_ptr eval_if() {
    // (if condition true-form false-form) 
    // where false-form is optional.
    SchemeObject* p = global_arg1;

    SchemeObject* s_condition_expr = s_car(p);;

    global_arg1 = s_condition_expr;
    bool condition = trampoline((fn_ptr)&eval)->boolValue();
	
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
    
    SchemeObject* result = S_TRUE;
    while (p != S_EMPTY_LIST) {
        if (s_cdr(p) == S_EMPTY_LIST) {
            // Tail call
            global_arg1 = s_car(p);
            return (fn_ptr)&eval;
        } else {
            global_arg1 = s_car(p);
            result = trampoline((fn_ptr)&eval);

            if (!result->boolValue()) {
                global_ret = result;
                return NULL;
            }
        } 
        p = s_cdr(p);
    }
    global_ret = result;
    return NULL;
}

fn_ptr eval_or() {
    SchemeObject* p = global_arg1;
    
    SchemeObject* result = S_FALSE;
    while (p != S_EMPTY_LIST) {
        if (s_cdr(p) == S_EMPTY_LIST) {
            // Optimize tail-recursion
            global_arg1 = s_car(p);
            return (fn_ptr)&eval;
        }
        
        global_arg1 = s_car(p);
        result = trampoline((fn_ptr)&eval);

        if (result->boolValue()) {
            global_ret = result;
            return NULL;
        }
        p = s_cdr(p);
    } 
    global_ret = result;
    return NULL;
}

fn_ptr eval_quasiquote() {
    return NULL;
}

/*
EVAL_QUASIQUOTE: {
    SchemeObject* o = tstack->popSchemeObject();
    BindingEnvironment* envt = tstack->popBindingEnvironment();
    SchemeObject* p = o;
    if (s_vector_p(o) == S_TRUE) {
        p = s_vector_2_list(o);
    } else {
        p = o;
    }
    if (s_pair_p(p) == S_TRUE) {
        SchemeObject* result = NULL;
        
        if (s_car(p) == unquote_symbol && s_vector_p(o) == S_FALSE) {
            tstack->push(envt);
            tstack->push(p);
            tstack->push(o);
            call_and_return(envt, s_cdr(p), EVAL);
            result = tstack->popSchemeObject();
            o = tstack->popSchemeObject(); // Pop local var
            p = tstack->popSchemeObject(); // Pop local var
            envt = tstack->popBindingEnvironment(); // Pop local var
        } else if (s_car(p) == unquote_splicing_symbol && s_vector_p(o) == S_FALSE) {
            tstack->push(envt);
            tstack->push(p);
            tstack->push(o);
            call_and_return(envt, s_cdr(p), EVAL);
            result = tstack->popSchemeObject();
            o = tstack->popSchemeObject(); // Pop local var
            p = tstack->popSchemeObject(); // Pop local var
            envt = tstack->popBindingEnvironment(); // Pop local var
            if (s_list_p(result) == S_FALSE) {
                throw scheme_exception("unquote-splicing must result in a list");
            }
        } else {
            // (car p) is a list or vector that we recurse into
            collected = S_EMPTY_LIST;
            p = s_car(p);
            while(true) {
                
                
                p = s_cdr(p);
                if (s_pair_p(p) == S_TRUE) {
                    s_set_car_e(resultp, r);
                    s_set_cdr_e(resultp) = cons(r,S_EMPTY_LIST);
                    resultp = s_cdr(resultp);
                } else {
                    s_set_cdr_e(resultp) = r;
                    break;
                }
            }
        }

        if (s_vector_p(o) == S_TRUE) {
            tstack->return_jump(s_list_2_vector(result));
        } else {
            tstack->return_jump(result);
        }
        tstack->return_jump(result);
        
        while(s_null_p(p) == S_FALSE) {
            SchemeObject* to_add = NULL;
            cout << "s_car(p) is " << s_car(p)->toString() << endl;
            if (s_pair_p(s_car(p)) == S_TRUE || s_vector_p(s_car(p)) == S_TRUE) {
                if (s_pair_p(s_car(p)) == S_TRUE && s_symbol_p(s_car(p)) == S_TRUE) {
                    cout << "Yup: " << s_car(s_car(p))->toString() << endl;
                }
                // (car p) is a list or vector that we recurse into
                tstack->push(envt);
                tstack->push(p);
                tstack->push(o);
                tstack->push(result);
                call_and_return(envt, s_car(p), EVAL_QUASIQUOTE);
                to_add = tstack->popSchemeObject();
                result = tstack->popSchemeObject(); // Pop local var
                o = tstack->popSchemeObject(); // Pop local var
                p = tstack->popSchemeObject(); // Pop local var
                envt = tstack->popBindingEnvironment(); // Pop local var

                result = s_cons(to_add, result);
                p = s_cdr(p);
                continue;
            } else {
                // (car p) is neither list nor vector
                result = s_cons(s_car(p), result);
                p = s_cdr(p);
                continue;
            }
        }
        result = s_reverse(result);
    } else {
        tstack->return_jump(o);
    }
}
*/

fn_ptr eval_procedure_call() {
    SchemeProcedure* proc = static_cast<SchemeProcedure*>(global_arg1);
    SchemeObject* args = global_arg2;

    SchemeObject* result = S_UNSPECIFIED;

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
            // TODO: append_e nogle S_UNSPECIFIED på args, så længden af args bliver req+opt.
        }
        SchemeObject* a_p = args;
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
        global_ret = result;
        return NULL;
    } else {
        // User function
        BindingEnvironment* new_envt = new BindingEnvironment(proc->envt);
        SchemeObject* req_symbols = proc->s_req;
        while (req_symbols != S_EMPTY_LIST) {
            if (args == S_EMPTY_LIST) {
                throw scheme_exception("Too few argument given.");
            }
            new_envt->put(static_cast<SchemeSymbol*>(s_car(req_symbols)), s_car(args));
            req_symbols = s_cdr(req_symbols);
            args = s_cdr(args);
        }
        if (proc->rst == 0 && args != S_EMPTY_LIST) {
            throw scheme_exception("Too many argument given.");
        }
        if (proc->rst == 1) {
            new_envt->put(proc->s_rst, args);
        }
        global_envt = new_envt;
        global_arg1 = proc->s_body;
        return (fn_ptr)&eval_sequence;
    }    
}

fn_ptr eval_multi() {
    // TODO: Facilitate allocation of multiple cells at once instead
    // of separate calls to s_cons.
    SchemeObject* p = global_arg1;

    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* tail_pair = S_EMPTY_LIST;
    while (p != S_EMPTY_LIST) {
        global_arg1 = s_car(p);
        SchemeObject* r = trampoline((fn_ptr)&eval);

	if (result == S_EMPTY_LIST) {
	    tail_pair = result = s_cons(r, S_EMPTY_LIST);
	} else {
	    SchemeObject* new_tail = s_cons(r, S_EMPTY_LIST);
	    s_set_cdr_e(tail_pair, new_tail);
	    tail_pair = new_tail;
	}

        p = s_cdr(p);
    }
    global_ret = result;
    return NULL;
}

fn_ptr eval_lambda() {
    SchemeObject* formals = global_arg1;
    SchemeObject* body = global_arg2;
    BindingEnvironment* envt = global_envt;

    SchemeSymbol* rst;
    SchemePair* req;
    if (s_symbol_p(formals) == S_TRUE) {
        rst = static_cast<SchemeSymbol*>(formals);
        req = S_EMPTY_LIST;
    } else if (s_pair_p(formals) == S_TRUE) {
        req = S_EMPTY_LIST;
        while (s_pair_p(formals) == S_TRUE) {
            SchemePair* pp = static_cast<SchemePair*>(formals);
            if (s_symbol_p(s_car(pp)) == S_FALSE) {
                throw scheme_exception("Bad formals");                
            }
            req = s_cons(s_car(pp), req);
            formals = s_cdr(pp);
        }
        req = s_reverse(req);
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
        req = S_EMPTY_LIST;
        rst = NULL;
    } else {
        throw scheme_exception("Bad formals");
    }

    global_ret = new SchemeProcedure(envt, req, rst, body);
    return NULL;
}

fn_ptr eval_apply() {
    // args is a list (proc arg1 arg2 ... argn). argn must be a list. proc is called with the arguments
    // (append (list arg1 arg2 ...) argn)
    SchemeObject* args = global_arg1;
    
    // Eval the procedure argument
    global_arg1 = s_car(args);
    SchemeObject* proc = trampoline((fn_ptr)&eval);

    if (s_procedure_p(proc) == S_FALSE) {
        throw scheme_exception("Can't apply to non-procedure: " + s_car(args)->toString());
    }

    if (s_pair_p(s_cdr(args)) == S_FALSE) {
        throw scheme_exception("Wrong type in position 1.");
    }

    // Eval and join all arg-lists together
    global_arg1 = s_cdr(args);
    args = trampoline((fn_ptr)&eval_multi);
    
    SchemePair* collected = S_EMPTY_LIST;
    SchemePair* prev = NULL;
    while (args != S_EMPTY_LIST) {
        SchemeObject* arg = s_car(args);
        if (s_pair_p(arg) == S_TRUE || arg == S_EMPTY_LIST) {
            if (s_cdr(args) == S_EMPTY_LIST) {
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
        args = s_cdr(args);
    }
    
    global_arg1 = proc;
    global_arg2 = collected;
    return (fn_ptr)&eval_procedure_call;
}


fn_ptr eval_set_e() {
    // TODO: We're doing double hashtable lookups. Both a envt->get(s) and in envt->set(s,v). Optimize by letting envt->get(s) return a binding, that
    // we manipulate instead of doing the set(s,v)

    SchemeObject* p = global_arg1;
    BindingEnvironment* envt = global_envt;

    if (s_length(p) != S_TWO) {
        throw scheme_exception("Missing or extra expression");
    }
    SchemeObject* car = s_car(p);
    if (car->type() != SchemeObject::SYMBOL) {
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

fn_ptr eval_map() {
    SchemeObject* p = global_arg1;

    // TODO: Tjek at alle argument-lists er lige lange

    // Eval the procedure argument
    global_arg1 = s_car(p);
    SchemeObject* proc = trampoline((fn_ptr)&eval);
    
    if (s_procedure_p(proc) == S_FALSE) {
        throw scheme_exception("Can't apply to non-procedure: " + s_car(p)->toString());
    }
    
    p = s_cdr(p);
    
    if (p == S_EMPTY_LIST) {
        throw scheme_exception("Wrong number of arguments to map");
    }
    
    SchemeObject* lists = S_EMPTY_LIST;
    SchemeObject* prev = S_EMPTY_LIST;
    // Eval all the lists
    while (p != S_EMPTY_LIST) {
        
        global_arg1 = s_car(p);
        SchemeObject* list = trampoline((fn_ptr)&eval);

        if (s_pair_p(list) == S_FALSE) {
            throw scheme_exception("Wrong argument to map");
        }
        
        if (lists == S_EMPTY_LIST) {
            lists = s_cons(list,S_EMPTY_LIST);
            prev = lists;
        } else {
            SchemePair* tmp = s_cons(list,S_EMPTY_LIST);
            s_set_cdr_e(prev, tmp);
            prev = tmp;
        }
        p = s_cdr(p);
    }
        
    // Vi skralder af lists i hvert gennemløb. Så ((1 2 3)(10 20 30)) bliver til ((2 3)(20 30)) og til sidst ((3)(30))
    SchemePair* result = S_EMPTY_LIST;
    while (s_car(lists) != S_EMPTY_LIST) {
        // Collect args
        SchemePair* collection = S_EMPTY_LIST;
        SchemePair* prev_col = S_EMPTY_LIST;
        SchemeObject* lists_ptr = lists;
        while (lists_ptr != S_EMPTY_LIST) {
            SchemeObject* arg = s_car(s_car(lists_ptr));
            s_set_car_e(lists_ptr,s_cdr(s_car(lists_ptr)));
            if (collection == S_EMPTY_LIST) {
                collection = s_cons(arg,S_EMPTY_LIST);
                prev_col = collection;
            } else {
                SchemePair* tmp = s_cons(arg,S_EMPTY_LIST);
                prev_col->cdr = tmp;
                prev_col = tmp;
                
            }
            lists_ptr = s_cdr(lists_ptr);
        }
        
        global_arg1 = proc;
        global_arg2 = collection;
        SchemeObject* result_item = trampoline((fn_ptr)&eval_procedure_call);
        
        if (result == S_EMPTY_LIST) {
            result = s_cons(result_item, S_EMPTY_LIST);
            prev = result;
        } else {
            SchemePair* tmp = s_cons(result_item, S_EMPTY_LIST);
            s_set_cdr_e(prev, tmp);
            prev = tmp;
        }
    }
    // Tjek at argumentlisterne var lige lange
    while (lists != S_EMPTY_LIST) {
        if (s_car(lists) != S_EMPTY_LIST) {
            throw scheme_exception("Argument lists not equals length.");
        }
        lists = s_cdr(lists);
    }
    
    global_ret = result;
    return NULL;
}

fn_ptr eval_for_each() {
    // TODO: This is a hack, but it works. The correct and fast impl. is to reuse 
    // the map-evaluator-code above but don't collect the result list.        
    trampoline((fn_ptr)&eval_map);
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
            if (s_car(s_cdr(clause)) == ergo_symbol) {
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
            global_arg1 = s_cdr(clause);
            return (fn_ptr)&eval_sequence;
        } else if (s_pair_p(clause_car) == S_TRUE) {
            if (s_memv(key,clause_car) != S_FALSE) {
                global_arg1 = s_cdr(clause);
                return (fn_ptr)&eval_sequence;
            }
        } else {
            throw scheme_exception("Invalid clause in case-statement");
        }

        p = s_cdr(p);
    }
    global_ret = S_UNSPECIFIED;
    return NULL;    
}

fn_ptr eval_let() {
    SchemeObject* p = global_arg1;
    BindingEnvironment* envt = global_envt;

    if (s_symbol_p(s_car(p)) == S_TRUE) {
        return (fn_ptr)&eval_named_let;
    }
    if (s_pair_p(s_car(p)) == S_FALSE && s_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad body in let");
    }
    
    // Build new bindings
    BindingEnvironment* new_bindings = new BindingEnvironment(envt);
    SchemeObject* binding_pairs = s_car(p);

    while (s_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        
        global_arg1 = s_car(s_cdr(s_car(binding_pairs)));
        SchemeObject* val = trampoline((fn_ptr)&eval);

        new_bindings->put(static_cast<SchemeSymbol*>(s_car(s_car(binding_pairs))), val);
        binding_pairs = s_cdr(binding_pairs);
    }
    
    global_envt = new_bindings;
    global_arg1 = s_cdr(p);
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_named_let() {
    SchemeObject* p = global_arg1;
    BindingEnvironment* envt = global_envt;

    SchemeObject* name = s_car(p);
    p = s_cdr(p);
    
    if (s_pair_p(s_car(p)) == S_FALSE && s_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad formals in let");
    }
    
    // Extract formals and collect evaluated args for a lambda
    SchemeObject* formals = S_EMPTY_LIST;
    SchemeObject* args = S_EMPTY_LIST;
    SchemeObject* binding_pairs = s_car(p);

    while (s_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        global_arg1 = s_car(s_cdr(s_car(binding_pairs)));
        SchemeObject* val = trampoline((fn_ptr)&eval);
        
        formals = s_cons(s_car(s_car(binding_pairs)), formals);
        args = s_cons(val, args);
        
        binding_pairs = s_cdr(binding_pairs);
    }

    BindingEnvironment* new_envt = new BindingEnvironment(envt);
    SchemeProcedure* lambda = new SchemeProcedure(new_envt, s_reverse(formals), NULL, static_cast<SchemePair*>(s_cdr(p)));
    new_envt->put(static_cast<SchemeSymbol*>(name), lambda);
    
    global_arg1 = lambda;
    global_arg2 = s_reverse(args);
    global_envt = new_envt;
    return (fn_ptr)&eval_procedure_call;
}

fn_ptr eval_letstar() {
    SchemeObject* p = global_arg1;
    BindingEnvironment* envt = global_envt;
    
    if (s_pair_p(s_car(p)) == S_FALSE && s_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad formals in let*: " + s_car(p)->toString());
    }
    
    if (s_null_p(s_cdr(p)) == S_TRUE) {
        throw scheme_exception("Missing body in let*");
    }

    // Build new bindings
    BindingEnvironment* new_bindings = new BindingEnvironment(envt);
    SchemeObject* binding_pairs = s_car(p);

    while (s_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        global_arg1 = s_car(s_cdr(s_car(binding_pairs)));
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
    BindingEnvironment* envt = global_envt;

    SchemeObject* formals = s_car(p);
    SchemePair* body = static_cast<SchemePair*>(s_cdr(p));
    SchemeObject* name = s_car(formals);
    formals = s_cdr(formals);
    
    if (s_symbol_p(name) == S_FALSE) {
        throw scheme_exception("Invalid macro-name in definition");
    }
    
    SchemeSymbol* rst;
    SchemePair* req;
    if (s_pair_p(formals) == S_TRUE) {
        req = S_EMPTY_LIST;
        while (s_pair_p(formals) == S_TRUE) {
            SchemePair* pp = static_cast<SchemePair*>(formals);
            if (s_symbol_p(s_car(pp)) == S_FALSE) {
                throw scheme_exception("Bad formals");                
            }
            req = s_cons(s_car(pp), req);
            formals = s_cdr(pp);
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
    
    SchemeMacro* macro = new SchemeMacro(envt, req, rst, body);
    envt->put(static_cast<SchemeSymbol*>(name), macro);
    
    global_ret = S_UNSPECIFIED;
    return (fn_ptr)NULL;
}

fn_ptr eval_call_macro() {
    SchemeMacro* proc = static_cast<SchemeMacro*>(global_arg1);
    SchemeObject* args = global_arg2;
    BindingEnvironment* envt = global_envt;
    
    // Build new environment
    BindingEnvironment* new_envt = new BindingEnvironment(proc->envt);
    SchemeObject* req_symbols = proc->s_req;
    while (req_symbols != S_EMPTY_LIST) {
        if (args == S_EMPTY_LIST) {
            throw scheme_exception("Too few argument given.");
        }
        new_envt->put(static_cast<SchemeSymbol*>(s_car(req_symbols)), s_car(args));
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
    global_envt = new_envt;
    global_arg1 = proc->s_body;
    SchemeObject* transformed_body = trampoline((fn_ptr)&eval_sequence);
    // cout << "Transformed body: " << transformed_body->toString() << endl;
    
    // Eval transformed body
    global_envt = envt;
    global_arg1 = transformed_body;
    return (fn_ptr)&eval;    
}

fn_ptr eval_do() {
    SchemeObject* p = global_arg1;
    BindingEnvironment* envt = global_envt;

    if (s_pair_p(s_car(p)) == S_FALSE && s_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad body in let");
    }

    // Extract formals and collect evaluated args for a lambda
    BindingEnvironment* new_envt = new BindingEnvironment(envt);
    SchemeObject* steps = S_EMPTY_LIST;
    SchemeObject* varnames = S_EMPTY_LIST;
    SchemeObject* binding_pairs = s_car(p);

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


        SchemeObject* step = s_cdr(s_cdr(s_car(binding_pairs)));
        if (step == S_EMPTY_LIST) {
            step = varname;
        } else {
            step = s_car(step);
        }
        steps = s_cons(step, steps);

        binding_pairs = s_cdr(binding_pairs);
    }
    
    SchemeObject* body = s_cdr(p);
    
    while (true) {
        // Evaluate test
        global_arg1 = s_car(s_car(body));
        global_envt = new_envt;
        SchemeObject* val = trampoline((fn_ptr)&eval);
        global_envt = envt;

        // Return if test is true
        if (val->boolValue()) {
            if (s_cdr(s_car(body)) == S_EMPTY_LIST) {
                global_ret = S_UNSPECIFIED;
                return NULL;
            } else {
                global_envt = new_envt;
                global_arg1 = s_cdr(s_car(body));
                return (fn_ptr)&eval_sequence;
            }
        }
        
        if (s_cdr(body) != S_EMPTY_LIST) {
            global_envt = new_envt;
            global_arg1 = s_cdr(body);
            trampoline((fn_ptr)&eval_sequence);
            global_envt = new_envt;
        }
        
        // Evaluate steps
        global_envt = new_envt;
        global_arg1 = steps;
        SchemeObject* vals = trampoline((fn_ptr)&eval_multi);
        global_envt = envt;
        
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
