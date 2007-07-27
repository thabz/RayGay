
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

SchemeObject* global_ret;
SchemeObject* global_arg1;
SchemeObject* global_arg2;
SchemeObject* global_arg3;
SchemeObject* global_envt;
vector<SchemeObject*> stack;


//------------------------------------------------------------------------
// Interpreter
//------------------------------------------------------------------------
Interpreter::Interpreter(SchemeObject* parsetree, SchemeObject* top_level_bindings) {
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
    global_arg2 = i_cons(arg,S_EMPTY_LIST);
    stack.push_back(global_arg1);
    stack.push_back(global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call);
    stack.pop_back();
    stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_2(SchemeObject* procedure, SchemeObject* arg1, SchemeObject* arg2) {
    global_arg1 = procedure;
    global_arg2 = i_cons(arg1, i_cons(arg2, S_EMPTY_LIST));
    stack.push_back(global_arg1);
    stack.push_back(global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call);
    stack.pop_back();
    stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_3(SchemeObject* procedure, SchemeObject* arg1, SchemeObject* arg2, SchemeObject* arg3) {
    global_arg1 = procedure;
    global_arg2 = i_cons(arg1, i_cons(arg2, i_cons(arg3, S_EMPTY_LIST)));
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
    SchemeObject* result;
    try {
        result = trampoline((fn_ptr)&eval_sequence);
    } catch (scheme_exception e) {
        stack.pop_back();
        stack.pop_back();
        throw e;
    }
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
    SchemeObject* saved = global_envt;
    size_t stack_size = stack.size();
    stack.push_back(saved);
    try {
        while (f != NULL) {
            f = (fn_ptr)(*f)();
        }
    } catch (scheme_exception e) {
        global_envt = saved;
        stack.resize(stack_size);
        throw e;
    }
    stack.pop_back();
    global_envt = saved;
    return global_ret;
}

fn_ptr eval() {
    SchemeObject* s = global_arg1;
    SchemeObject::ObjectType type = s->type();
    
    if (type < SchemeObject::SELF_EVALUATING_FORMS_ARE_BEFORE_HERE) {
        global_ret = s;
        return NULL;
    } else if (type == SchemeObject::SYMBOL) {
        SchemeObject* symbol = s;
	s = global_envt->getBinding(symbol);
        if (s == NULL) {
            throw scheme_exception("Unbound variable " + string(symbol->str));
        }
        global_ret = s;
        return NULL;
    } else if (type == SchemeObject::PAIR) {
        global_arg1 = s;
        return (fn_ptr)&eval_list;
    } else {
        throw scheme_exception("Unknown type: " + s->toString());
    }
}

fn_ptr eval_list() {
    SchemeObject* p = global_arg1;
    SchemeObject* envt = global_envt;

    Heap* heap = Heap::getUniqueInstance();
    if (heap->timeToGarbageCollect()) {
        heap->addRoot(p);
        heap->addRoot(envt);
        heap->garbageCollect(stack);
        heap->popRoot();
        heap->popRoot();
    }

    SchemeObject* car = i_car(p);
    if (i_symbol_p(car) == S_FALSE) {
        return (fn_ptr)&eval_combo;
    }

    SchemeObject* s = car;
    SchemeObject* cdr = i_cdr(p);
	
    SchemeObject* proc = envt->getBinding(s);
    if (proc != NULL) {
        if (proc->type() == SchemeObject::USER_PROCEDURE) {
            global_arg1 = proc;
            global_arg2 = cdr;
            return (fn_ptr)&eval_user_procedure_call;
        } else if (proc->type() == SchemeObject::BUILT_IN_PROCEDURE) {
            global_arg1 = proc;
            global_arg2 = cdr;
            return (fn_ptr)&eval_built_in_procedure_call;
        } else if (proc->type() == SchemeObject::CONTINUATION) {
            global_arg1 = i_car(cdr);
            eval();
            proc->callContinuation(global_ret);
        } else if (proc->type() == SchemeObject::MACRO) {
            global_arg1 = proc;
            global_arg2 = cdr;
            return (fn_ptr)&eval_call_macro;
        } else if (proc->type() == SchemeObject::INTERNAL_PROCEDURE) {
            if (s == if_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_if;
            } else if (s == quote_symbol) {
                global_ret = i_car(cdr);
                return NULL;
            } else if (s == define_symbol) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_define;
            } else if (s == define_macro) {
                global_arg1 = cdr;
                return (fn_ptr)&eval_define_macro;
            } else if (s == quasiquote_symbol) {
                global_arg1 = i_car(cdr);
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
    return NULL; // Never reached
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
    if (i_cdr(p) == S_EMPTY_LIST) {
        // List has one element. Do a tail call.
        global_arg1 = i_car(p);
        return (fn_ptr)&eval;
    }
    stack.push_back(p);
    stack.push_back(global_envt);
    while (true) {
        if (i_cdr(p) == S_EMPTY_LIST) {
            // The tail call, let EVAL return to this' caller
            stack.pop_back();
            stack.pop_back();
            global_arg1 = i_car(p);
            return (fn_ptr)&eval;
        } else {
            global_arg1 = i_car(p);
            trampoline((fn_ptr)&eval);
            p = i_cdr(p);
        }
    }    
}

//
// Evals a list of expressions and the returns the list of results
//
fn_ptr eval_multi() {
    SchemeObject* p = global_arg1;
    stack.push_back(p);
    stack.push_back(global_envt);
    
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* result_tail = S_EMPTY_LIST;
    stack.push_back(result);
    SchemeObject** result_stack_ref = &(stack.back());
    
    while (p != S_EMPTY_LIST) {
        global_arg1 = i_car(p);
        SchemeObject* r;
        if (global_arg1->self_evaluating()) {
            r = global_arg1;
        } else {
            r = trampoline((fn_ptr)&eval);
        }

        if (result == S_EMPTY_LIST) {
            result = i_cons(r, S_EMPTY_LIST);
            result_tail = result;
            *result_stack_ref = result;
        } else {
            SchemeObject* new_tail = i_cons(r, S_EMPTY_LIST);
            i_set_cdr_e(result_tail, new_tail);
            result_tail = new_tail;
        }

        p = i_cdr(p);
    }
    stack.pop_back();
    stack.pop_back();
    stack.pop_back();
    global_ret = result;
    return NULL;
}


fn_ptr eval_define() {
    SchemeObject* p = global_arg1;
    SchemeObject* envt = global_envt;
    
    if (p == S_EMPTY_LIST || i_car(p) == S_EMPTY_LIST || i_cdr(p) == S_EMPTY_LIST) {
        throw scheme_exception("Missing arguments to define: " + p->toString());
    }
    
    if (i_pair_p(i_car(p)) == S_TRUE) {
        // (define (func-name args...) body-forms...)
        SchemeObject* pa = i_car(p);
        if (i_symbol_p(i_car(pa)) == S_FALSE) {
            throw scheme_exception("Bad variable");
        }
        SchemeObject* body = i_cdr(p);
        SchemeObject* name = i_car(pa);

        global_arg1 = i_cdr(pa);
        global_arg2 = body;
        global_arg3 = name;
        trampoline((fn_ptr)&eval_lambda);
        SchemeObject* proc = global_ret;
        
        envt->defineBinding(name , proc);
    } else {
        // (define var value-expr)
        if (i_cddr(p) != S_EMPTY_LIST) {
            throw scheme_exception("Extra arguments to define: " + p->toString());
        }
        
        SchemeObject* s = i_car(p);
        if (i_symbol_p(s) == S_FALSE) {
            throw scheme_exception("Bad variable");
        }
        
        global_arg1 = i_car(i_cdr(p));
        SchemeObject* v = trampoline((fn_ptr)&eval);

        envt->defineBinding(s, v);
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

    SchemeObject* collected = S_EMPTY_LIST;
    SchemeObject* prev = NULL;
    int i = 0;
    while (args != S_EMPTY_LIST) {
        i++;
        SchemeObject* arg = s_car(args);
        if (i_pair_p(arg) == S_TRUE || arg == S_EMPTY_LIST) {
            if (s_cdr(args) == S_EMPTY_LIST) {
                // arg is a list and last argument
                if (collected == S_EMPTY_LIST) {
                    collected = arg;
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
                SchemeObject* tmp = s_cons(arg,S_EMPTY_LIST);
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
// where form is an expression that should evaluate to a procedure that we execute
//
fn_ptr eval_combo() {
    SchemeObject* s = global_arg1;
    stack.push_back(s);
    
    global_arg1 = s_car(s);
    SchemeObject* proc = trampoline((fn_ptr)&eval);
    
    if (i_procedure_p(proc) == S_FALSE) {
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
    bool condition = scm2bool(trampoline((fn_ptr)&eval));
    stack.pop_back();
	
    if (condition) {
        // Evaluate and return true case
        SchemeObject* true_case = s_car(s_cdr(p));
        global_arg1 = true_case;
        return (fn_ptr)&eval;
    } else if (s_cddr(p) != S_EMPTY_LIST) {
        // Evaluate and return false case
    	SchemeObject* false_case = s_caddr(p);
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
    
    SchemeObject *cur, *next, *result = S_TRUE;

    while (p != S_EMPTY_LIST) {
        cur = i_car(p);
        next = i_cdr(p);
        if (next == S_EMPTY_LIST) {
            // Tail call
            stack.pop_back();
            global_arg1 = cur;
            return (fn_ptr)&eval;
        } else {
            global_arg1 = cur;
            result = trampoline((fn_ptr)&eval);
            if (!scm2bool(result)) {
                stack.pop_back();
                global_ret = result;
                return NULL;
            }
            p = next;
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

            if (scm2bool(result)) {
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
    SchemeObject* p = o;
    if (s_vector_p(o) == S_TRUE) {
        p = s_vector_2_list(o);
    } else {
        p = o;
    }

    SchemeObject* result = S_EMPTY_LIST;

    if (i_pair_p(p) == S_TRUE) {
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
                if (i_pair_p(p) == S_TRUE) {
                    
                    if (s_car(p) == unquote_symbol) {
                        // Handle when final cdr of unproper list is a unquote
                        stack.push_back(result);
                        s_set_cdr_e(prev,eval_unquote_recursive(p, level));
                        stack.pop_back();
                        break;
                    }
                    
                    stack.push_back(p);
                    stack.push_back(result);
                    SchemeObject* r = eval_quasiquote_recursive(s_car(p),level);
                    stack.pop_back();
                    stack.pop_back();

                    if (i_pair_p(s_car(p)) == S_TRUE && s_car(s_car(p)) == unquote_splicing_symbol) {
                        // Splice into result
                        if (result == S_EMPTY_LIST) {
                            result = r;
                            prev = result;
                        } else {
                            s_set_cdr_e(prev,r);
                        }
                        // Forward-wind prev to point to end of newly added list
                        while(i_null_p(s_cdr(prev)) == S_FALSE) {
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
                    stack.push_back(result);
                    SchemeObject* r = eval_quasiquote_recursive(p, level);
                    stack.pop_back();
                    if (result != S_EMPTY_LIST) {
                        s_set_cdr_e(prev,r);
                    } else {
                        result = r;  
                    }
                    break;
                }
            }
        }

        if (s_vector_p(o) == S_TRUE) {
            return s_list_2_vector(result);
        } else {
            return result;
        }
    } else {
        return o;
    }
}

fn_ptr eval_quasiquote() {
    global_ret = eval_quasiquote_recursive(global_arg1,0);
    return NULL;
}

fn_ptr eval_user_procedure_call() {
    SchemeObject* proc = global_arg1;
    SchemeObject* args_to_eval = global_arg2;
    
    assert(proc != NULL);

    SchemeObject* new_envt = SchemeObject::createEnvironment(proc->s_envt());
    SchemeObject* formals = proc->s_formals();

    stack.push_back(proc);
    stack.push_back(args_to_eval);
    stack.push_back(new_envt);
    
    while (i_pair_p(formals) == S_TRUE) {
        if (args_to_eval == S_EMPTY_LIST) {
            throw scheme_exception("Too few argument given in call to "+proc->nameAsString());
        }
        
        global_arg1 = i_car(args_to_eval);
        SchemeObject* arg = trampoline((fn_ptr)&eval);
        
        new_envt->defineBinding(i_car(formals), arg);
        args_to_eval = i_cdr(args_to_eval);
        formals = i_cdr(formals);
    }
    if (formals != S_EMPTY_LIST) {
        // Rest argument    
        global_arg1 = args_to_eval;
        SchemeObject* args = trampoline((fn_ptr)&eval_multi);
        new_envt->defineBinding(formals, args);
    } else if (args_to_eval != S_EMPTY_LIST) {
        throw scheme_exception("Too many argument given in call to "+proc->nameAsString());
    }

    stack.pop_back();
    stack.pop_back();
    stack.pop_back();
    
    global_envt = new_envt;
    global_arg1 = proc->s_body();
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_built_in_procedure_call() 
{
    SchemeObject* proc = global_arg1;
    SchemeObject* args = global_arg2;
    
    assert(proc != NULL);

    stack.push_back(proc);
    stack.push_back(args);

    SchemeObject* result = S_UNSPECIFIED;

    // Built-in function
    int req = proc->req();
    int opt = proc->opt();
    
    for(int i = 0; i < req; i++) {
        if (args == S_EMPTY_LIST) {
            throw scheme_exception("Too few argument given in call to "+proc->nameAsString());
        }
        global_arg1 = i_car(args);
        SchemeObject* arg = trampoline((fn_ptr)&eval);
        stack.push_back(arg);
        args = i_cdr(args);
    }
    for(int i = 0; i < opt; i++) {
        if (args != S_EMPTY_LIST) {
            global_arg1 = i_car(args);
            SchemeObject* arg = trampoline((fn_ptr)&eval);
            stack.push_back(arg);
            args = i_cdr(args);
        } else {
            stack.push_back(S_UNSPECIFIED);
        }
    }
    
    int num = req + opt;
    if (!proc->rest() && args != S_EMPTY_LIST) {
        throw scheme_exception("Too many argument given in call to "+proc->nameAsString());
    }
    if (proc->rest()) {
        while (args != S_EMPTY_LIST) {
            global_arg1 = i_car(args);
            SchemeObject* arg = trampoline((fn_ptr)&eval);
            stack.push_back(arg);
            args = i_cdr(args);
            num++;
        }    
    }
    
    try {
        SchemeStack::iterator stack_iter = stack.end() - num;

        if (proc->rest()) {
            result = (*((SchemeObject* (*)(int, SchemeStack::iterator))(proc->fn)))(num, stack_iter);
        } else {
            SchemeObject* argsv[num];
            for(int i = 0; i < num; i++) {
                argsv[i] = *stack_iter;
                stack_iter++;        
            }
            switch(num) {
                // TODO: Support up to 16+16 arguments. Or at least more than this.
                case 0:   result = (*((SchemeObject* (*)())(proc->fn)))();
                          break;
                case 1:   result = (*((SchemeObject* (*)(SchemeObject*))(proc->fn)))(argsv[0]);
                          break;
                case 2:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1]);
                          break;
                case 3:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2]);
                          break;
                case 4:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3]);
                          break;
                case 5:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3],argsv[4]);
                          break;
                case 6:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3],argsv[4],argsv[5]);
                          break;
                case 7:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3],argsv[4],argsv[5],argsv[6]);
                          break;
                case 8:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3],argsv[4],argsv[5],argsv[6],argsv[7]);
                          break;
                default:  throw scheme_exception("Doesn't support that many args to a built-in function."); 
            }
        }
    } catch (scheme_exception e) {
        string s = "In call to procedure " + proc->nameAsString() + ": " + e.toString();
        throw scheme_exception(s);
    }
    for(int i = 0; i < num; i++) {
        stack.pop_back();
    }
    stack.pop_back();
    stack.pop_back();
    global_ret = result;
    return NULL;    
}

fn_ptr eval_procedure_call() {
    SchemeObject* proc = global_arg1;
    SchemeObject* args = global_arg2;
    
    assert(proc != NULL);
    
    stack.push_back(proc);
    stack.push_back(args);
    
    SchemeObject* result = S_UNSPECIFIED;
    
    if (proc->type() == SchemeObject::BUILT_IN_PROCEDURE) {
        // Built-in function
        int req = proc->req();
        int opt = proc->opt();

        for(int i = 0; i < req; i++) {
            if (args == S_EMPTY_LIST) {
                throw scheme_exception("Too few argument given in call to "+proc->nameAsString());
            }
            stack.push_back(i_car(args));
            args = i_cdr(args);
        }
        for(int i = 0; i < opt; i++) {
            if (args != S_EMPTY_LIST) {
                stack.push_back(i_car(args));
                args = i_cdr(args);
            } else {
                stack.push_back(S_UNSPECIFIED);
            }
        }

        int num = req + opt;
        
        if (!proc->rest() && args != S_EMPTY_LIST) {
            throw scheme_exception("Too many argument given in call to "+proc->nameAsString());
        }
        if (proc->rest()) {
            while (args != S_EMPTY_LIST) {
                 stack.push_back(i_car(args));
                 args = i_cdr(args);
                 num++;
            }        
        }
        try {
            SchemeStack::iterator stack_iter = stack.end() - num;
            
            if (proc->rest()) {
                result = (*((SchemeObject* (*)(int, SchemeStack::iterator))(proc->fn)))(num, stack_iter);
            } else {
                SchemeObject* argsv[num];
                for(int i = 0; i < num; i++) {
                    argsv[i] = *stack_iter;
                    stack_iter++;        
                }        
                switch(num) {
                    // TODO: Support up to 16+16 arguments. Or at least more than this.
                    case 0:   result = (*((SchemeObject* (*)())(proc->fn)))();
                              break;
                    case 1:   result = (*((SchemeObject* (*)(SchemeObject*))(proc->fn)))(argsv[0]);
                              break;
                    case 2:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1]);
                              break;
                    case 3:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2]);
                              break;
                    case 4:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3]);
                              break;
                    case 5:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3],argsv[4]);
                              break;
                    case 6:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3],argsv[4],argsv[5]);
                              break;
                    case 7:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3],argsv[4],argsv[5],argsv[6]);
                              break;
                    case 8:   result = (*((SchemeObject* (*)(SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*,SchemeObject*))(proc->fn)))(argsv[0],argsv[1],argsv[2],argsv[3],argsv[4],argsv[5],argsv[6],argsv[7]);
                              break;
                    default:  throw scheme_exception("Doesn't support that many args to a built-in function."); 
                }
            }
        } catch (scheme_exception e) {
            string s = "In call to procedure " + proc->nameAsString() + ": " + e.toString();
            throw scheme_exception(s);
        }
        for(int i = 0; i < num; i++) {
            stack.pop_back();
        }
        stack.pop_back();
        stack.pop_back();
        global_ret = result;
        return NULL;
    } else {
        // User function
        SchemeObject* new_envt = SchemeObject::createEnvironment(proc->s_envt());
        SchemeObject* formals = proc->s_formals();

        while (i_pair_p(formals) == S_TRUE) {
            if (args == S_EMPTY_LIST) {
                throw scheme_exception("Too few argument given in call to "+proc->nameAsString());
            }
            new_envt->defineBinding(i_car(formals), i_car(args));
            args = i_cdr(args);
            formals = i_cdr(formals);
        }
        if (formals != S_EMPTY_LIST) {
            new_envt->defineBinding(formals, args);
        } else if (args != S_EMPTY_LIST) {
            throw scheme_exception("Too many argument given in call to "+proc->nameAsString());
        }

        global_envt = new_envt;
        global_arg1 = proc->s_body();
        stack.pop_back();
        stack.pop_back();
        return (fn_ptr)&eval_sequence;
    }    
}

fn_ptr eval_lambda() {
    // TODO: Memoize eval_lambda, ie. cache the result.        
    SchemeObject* formals = global_arg1;
    SchemeObject* body = global_arg2;
    SchemeObject* name = global_arg3;
    SchemeObject* envt = global_envt;

    global_ret = SchemeObject::createUserProcedure(name, envt, formals, body);
    return NULL;
}

fn_ptr eval_set_e() {
    SchemeObject* p = global_arg1;
    SchemeObject* envt = global_envt;

    if (p == S_EMPTY_LIST || i_cdr(p) == S_EMPTY_LIST || i_cddr(p) != S_EMPTY_LIST) {
        throw scheme_exception("Missing or extra arguments to set!");
    }
    SchemeObject* car = i_car(p);
    if (i_symbol_p(car) == S_FALSE) {
        throw scheme_exception("Wrong type argument in position 1.");
    }
    SchemeObject* s = car;

    global_arg1 = i_cadr(p);
    SchemeObject* v = trampoline((fn_ptr)&eval);

    envt->setBinding(s, v);

    global_ret = S_UNSPECIFIED;
    return NULL;
}

fn_ptr eval_cond() {
    SchemeObject* p = global_arg1;

    while (i_null_p(p) == S_FALSE) {
        SchemeObject* clause = s_car(p);
        if (i_pair_p(clause) == S_FALSE) {
            throw scheme_exception("Invalid clause");
        }
        SchemeObject* test_expr = s_car(clause);
        if (test_expr == else_symbol) {
            // Handle (else <expressions> ...)
            if (i_null_p(s_cdr(p)) == S_FALSE) {
                throw scheme_exception("else-clause must be last");
            }
            global_arg1 = s_cdr(clause);
            return (fn_ptr)&eval_sequence;
        }
        
        // Eval test_expr
        global_arg1 = test_expr;
        SchemeObject* test = trampoline((fn_ptr)&eval);
        
        if (scm2bool(test)) {
            if (s_cdr(clause) == S_EMPTY_LIST) {
                global_ret = test;
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

    while (i_null_p(p) == S_FALSE) {
        SchemeObject* clause = s_car(p);
        if (i_pair_p(clause) == S_FALSE) {
            throw scheme_exception("Invalid clause");
        }
        SchemeObject* clause_car = s_car(clause);
        if (clause_car == else_symbol) {
            // Handle (else <expressions> ...)
            if (i_null_p(s_cdr(p)) == S_FALSE) {
                throw scheme_exception("else-clause must be last");
            }
            stack.pop_back();
            global_arg1 = s_cdr(clause);
            return (fn_ptr)&eval_sequence;
        } else if (i_pair_p(clause_car) == S_TRUE) {
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
    SchemeObject* envt = global_envt;
    
    if (i_null_p(p) == S_TRUE) {
        throw scheme_exception("Bad body in let");
    }
    
    SchemeObject* first_arg = i_car(p);

    if (s_symbol_p(first_arg) == S_TRUE) {
        return (fn_ptr)&eval_named_let;
    }
    
    if (i_pair_p(first_arg) == S_FALSE && i_null_p(first_arg) == S_FALSE) {
        throw scheme_exception("Bad body in let");
    }
    
    // Build new bindings
    SchemeObject* new_bindings = SchemeObject::createEnvironment(envt);
    SchemeObject* binding_pairs = first_arg;
    
    stack.push_back(new_bindings);

    while (i_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        
        global_arg1 = s_car(s_cdr(s_car(binding_pairs)));
        SchemeObject* val = trampoline((fn_ptr)&eval);

        new_bindings->defineBinding(s_car(s_car(binding_pairs)), val);
        binding_pairs = s_cdr(binding_pairs);
    }
    stack.pop_back();
    
    global_envt = new_bindings;
    global_arg1 = s_cdr(p);
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_named_let() {
    SchemeObject* p = global_arg1;
    SchemeObject* envt = global_envt;
    
    SchemeObject* name = s_car(p);
    p = s_cdr(p);
    
    if (i_pair_p(s_car(p)) == S_FALSE && i_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad formals in let");
    }
    
    // Extract formals and collect args for a lambda
    SchemeObject* formals = S_EMPTY_LIST;
    SchemeObject* formals_tail = S_EMPTY_LIST;
    SchemeObject* args = S_EMPTY_LIST;
    SchemeObject* args_tail = S_EMPTY_LIST;

    SchemeObject* binding_pairs = s_car(p);

    while (i_null_p(binding_pairs) == S_FALSE) {
        SchemeObject* binding_pair = s_car(binding_pairs);
        SchemeObject* formal = s_car(binding_pair);
        SchemeObject* arg = s_car(s_cdr(binding_pair));
        
    	if (formals == S_EMPTY_LIST) {
    	    formals = i_cons(formal, S_EMPTY_LIST);
    	    formals_tail = formals;
    	} else {
    	    SchemeObject* tmp = i_cons(formal, S_EMPTY_LIST);
    	    i_set_cdr_e(formals_tail,tmp);
    	    formals_tail = tmp;
    	}

    	if (args == S_EMPTY_LIST) {
    	    args = i_cons(arg, S_EMPTY_LIST);
    	    args_tail = args;
    	} else {
    	    SchemeObject* tmp = i_cons(arg, S_EMPTY_LIST);
    	    i_set_cdr_e(args_tail, tmp);
    	    args_tail = tmp;
    	}

        binding_pairs = s_cdr(binding_pairs);
    }
    
    SchemeObject* new_envt = SchemeObject::createEnvironment(envt);
    SchemeObject* lambda = SchemeObject::createUserProcedure(name, new_envt, formals, s_cdr(p));
    new_envt->defineBinding(name, lambda);
    
    global_arg1 = lambda;
    global_arg2 = args;
    return (fn_ptr)&eval_user_procedure_call;
}

fn_ptr eval_letstar() {
    SchemeObject* p = global_arg1;
    SchemeObject* envt = global_envt;
    
    if (i_null_p(p) == S_TRUE) {
        throw scheme_exception("Bad body in let*");
    }
    
    if (i_pair_p(s_car(p)) == S_FALSE && i_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad formals in let*: " + s_car(p)->toString());
    }
    
    if (i_null_p(s_cdr(p)) == S_TRUE) {
        throw scheme_exception("Missing body in let*");
    }

    // Build new bindings
    SchemeObject* new_bindings = SchemeObject::createEnvironment(envt);
    SchemeObject* binding_pairs = s_car(p);

    while (i_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        global_arg1 = s_cadar(binding_pairs);
        global_envt = new_bindings;
        SchemeObject* val = trampoline((fn_ptr)&eval);
        
	SchemeObject* sym = s_car(s_car(binding_pairs));
	if (s_symbol_p(sym) == S_FALSE) {
	    throw scheme_exception("Bad variable in let*: " + s_car(s_car(binding_pairs))->toString());
	}
        new_bindings->defineBinding(sym, val);
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
    SchemeObject* envt = global_envt;

    SchemeObject* formals = s_car(p);
    SchemeObject* body = s_cdr(p);
    SchemeObject* name = s_car(formals);
    formals = s_cdr(formals);
    
    if (i_symbol_p(name) == S_FALSE) {
        throw scheme_exception("Invalid macro-name in definition: " + name->toString());
    }

    SchemeObject* macro = SchemeObject::createMacro(name, envt, formals, body);
    envt->defineBinding(name, macro);
    
    global_ret = S_UNSPECIFIED;
    return (fn_ptr) NULL;
}

fn_ptr eval_call_macro() {
    SchemeObject* proc = global_arg1;
    SchemeObject* args = global_arg2;
    SchemeObject* envt = global_envt;
    
    // Build new environment
    SchemeObject* new_envt = SchemeObject::createEnvironment(proc->s_envt());
    SchemeObject* formals = proc->s_formals();
    
    while (i_pair_p(formals) == S_TRUE) {
        if (args == S_EMPTY_LIST) {
            throw scheme_exception("Too few argument given in call to macro " + proc->nameAsString());
        }
        new_envt->defineBinding(i_car(formals), i_car(args));
        formals = i_cdr(formals);
        args = i_cdr(args);
    }
    if (formals != S_EMPTY_LIST) {
        new_envt->defineBinding(formals, args);
    } else if (args != S_EMPTY_LIST) {
        throw scheme_exception("Too many argument given in call to macro "+proc->nameAsString());
    }
    
    // cout << "Body: " << proc->s_body->toString() << endl;
    // Transform body
    stack.push_back(envt);
    stack.push_back(proc);
    stack.push_back(new_envt);
    global_envt = new_envt;
    global_arg1 = proc->s_body();
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
    SchemeObject* envt = global_envt;

    if (i_pair_p(s_car(p)) == S_FALSE && i_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception("Bad body in do");
    }

    // Extract formals and collect evaluated args for a lambda
    SchemeObject* new_envt = SchemeObject::createEnvironment(envt);
    SchemeObject* steps = S_EMPTY_LIST;
    SchemeObject* varnames = S_EMPTY_LIST;
    SchemeObject* binding_pairs = s_car(p);
    
    stack.push_back(p);
    stack.push_back(new_envt);
    stack.push_back(steps);
    SchemeObject*& steps_stack_pos = stack.back();
    stack.push_back(varnames);
    SchemeObject*& varnames_stack_pos = stack.back();

    while (i_null_p(binding_pairs) == S_FALSE) {
        SchemeObject* binding = i_car(binding_pairs);

        if (i_pair_p(binding) == S_FALSE) {
            throw scheme_exception("Invalid binding in do-form");
        }

        // Binding symbol
        SchemeObject* varname = i_car(binding);
        if (i_symbol_p(varname) == S_FALSE) {
            throw scheme_exception("Invalid variable in do: " + varname->toString());
        }
        
        if (i_cdr(binding) == S_EMPTY_LIST) {
            throw scheme_exception("In do: missing initial value for variable " + varname->toString());
        }

        // Eval initial binding value
        global_arg1 = i_cadr(binding);
        SchemeObject* val = trampoline((fn_ptr)&eval);
        
        new_envt->defineBinding(varname, val);
        varnames = i_cons(varname, varnames);
        varnames_stack_pos = varnames;

        // Save step expression
        SchemeObject* step = i_cddr(binding);
        if (step == S_EMPTY_LIST) {
            step = varname;
        } else {
            step = i_car(step);
        }
        steps = i_cons(step, steps);
        steps_stack_pos = steps;

        binding_pairs = i_cdr(binding_pairs);
    }
    
    SchemeObject* body = i_cdr(p);
    
    global_envt = new_envt;
    
    while (true) {
        // Evaluate test
        if (i_car(body) == S_EMPTY_LIST) {
            throw scheme_exception("Missing exit clause in do");
        }
        global_arg1 = i_caar(body);
        SchemeObject* val = trampoline((fn_ptr)&eval);

        // Return if test is true
        if (scm2bool(val)) {
            if (i_cdar(body) == S_EMPTY_LIST) {
                global_ret = S_UNSPECIFIED;
                stack.pop_back();
                stack.pop_back();
                stack.pop_back();
                stack.pop_back();
                return NULL;
            } else {
                global_arg1 = s_cdar(body);
                stack.pop_back();
                stack.pop_back();
                stack.pop_back();
                stack.pop_back();
                return (fn_ptr)&eval_sequence;
            }
        }
        
        if (i_cdr(body) != S_EMPTY_LIST) {
            global_arg1 = i_cdr(body);
            trampoline((fn_ptr)&eval_sequence);
        }
        
        // Evaluate steps
        global_arg1 = steps;
        SchemeObject* vals = trampoline((fn_ptr)&eval_multi);
        
        // Assign new step values
        SchemeObject* tmp = varnames;
        while(i_null_p(varnames) == S_FALSE) {
            new_envt->defineBinding(i_car(varnames), i_car(vals));
            varnames = i_cdr(varnames);
            vals = i_cdr(vals);
        }
        varnames = tmp;
    }
    return NULL; /* Never reached */
}
