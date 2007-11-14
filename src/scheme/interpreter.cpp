// --- C++ ---
#include "interpreter.h"
#include "scheme.h"

#include <iostream>

using namespace std;

//------------------------------------------------------------------------
// Interpreter
//------------------------------------------------------------------------
Interpreter::Interpreter() {
    pthread_key_create(&state_key,NULL);	
}

SchemeObject* Interpreter::call_procedure_n(SchemeObject* procedure, SchemeObject* args) {
    State* state = getState();
    state->global_arg1 = procedure;
    state->global_arg2 = args;
    state->stack.push_back(state->global_arg1);
    state->stack.push_back(state->global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call, state);
    state->stack.pop_back();
    state->stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_0(SchemeObject* procedure) {
    State* state = getState();
    state->global_arg1 = procedure;
    state->global_arg2 = S_EMPTY_LIST;
    state->stack.push_back(state->global_arg1);
    state->stack.push_back(state->global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call, state);
    state->stack.pop_back();
    state->stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_1(SchemeObject* procedure, SchemeObject* arg) {
    State* state = getState();
    state->global_arg1 = procedure;
    state->global_arg2 = i_cons(arg,S_EMPTY_LIST);
    state->stack.push_back(state->global_arg1);
    state->stack.push_back(state->global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call, state);
    state->stack.pop_back();
    state->stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_2(SchemeObject* procedure, SchemeObject* arg1, SchemeObject* arg2) {
    State* state = getState();
    state->global_arg1 = procedure;
    state->global_arg2 = i_cons(arg1, i_cons(arg2, S_EMPTY_LIST));
    state->stack.push_back(state->global_arg1);
    state->stack.push_back(state->global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call, state);
    state->stack.pop_back();
    state->stack.pop_back();
    return result;
}

SchemeObject* Interpreter::call_procedure_3(SchemeObject* procedure, SchemeObject* arg1, SchemeObject* arg2, SchemeObject* arg3) {
    State* state = getState();
    state->global_arg1 = procedure;
    state->global_arg2 = i_cons(arg1, i_cons(arg2, i_cons(arg3, S_EMPTY_LIST)));
    state->stack.push_back(state->global_arg1);
    state->stack.push_back(state->global_arg2);
    SchemeObject* result = trampoline((fn_ptr)&eval_procedure_call, state);
    state->stack.pop_back();
    state->stack.pop_back();
    return result;
}


SchemeObject* Interpreter::interpret(SchemeObject* parsetree, SchemeObject* top_level_bindings) {
    State* state = getState();
    if (parsetree == S_EMPTY_LIST) {
	    return S_UNSPECIFIED;
    }
    state->stack.push_back(parsetree);
    state->stack.push_back(top_level_bindings);
    state->global_arg1 = parsetree;
    state->global_envt = top_level_bindings;
    SchemeObject* result;
    try {
        result = trampoline((fn_ptr)&eval_sequence, state);
    } catch (scheme_exception e) {
        state->stack.pop_back();
        state->stack.pop_back();
        throw e;
    }
    state->stack.pop_back();
    state->stack.pop_back();
    return result;
}

Interpreter::State* Interpreter::getState() {
    State* state = (State*) pthread_getspecific(state_key);
    if (state == NULL) {
        state = new Interpreter::State();    
        pthread_setspecific(state_key, state);
    }
    return state;
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

SchemeObject* trampoline(fn_ptr f, Interpreter::State* state) {
    SchemeObject* saved = state->global_envt;
    size_t stack_size = state->stack.size();
    state->stack.push_back(saved);
    try {
        while (f != NULL) {
            f = (fn_ptr)(*f)(state);
        }
    } catch (scheme_exception e) {
        state->global_envt = saved;
        state->stack.resize(stack_size);
        throw e;
    }
    state->stack.pop_back();
    state->global_envt = saved;
    return state->global_ret;
}

fn_ptr eval(Interpreter::State* state) {
    SchemeObject* s = state->global_arg1;
    SchemeObject::ObjectType type = s->type();
    
    if (type < SchemeObject::SELF_EVALUATING_FORMS_ARE_BEFORE_HERE) {
        state->global_ret = s;
        return NULL;
    } else if (type == SchemeObject::SYMBOL) {
        SchemeObject* symbol = s;
	    s = state->global_envt->getBinding(symbol);
        if (s == NULL) {
            throw scheme_exception(symbol->src_line(), L"Unbound variable " + wstring(symbol->str));
        }
        state->global_ret = s;
        return NULL;
    } else if (type == SchemeObject::PAIR) {
        state->global_arg1 = s;
        return (fn_ptr)&eval_list;
    } else {
        throw scheme_exception(L"Unknown type: " + s->toString());
    }
}

fn_ptr eval_list(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    SchemeObject* envt = state->global_envt;

    Heap* heap = Heap::getUniqueInstance();
    if (heap->timeToGarbageCollect()) {
        heap->addRoot(p);
        heap->addRoot(envt);
	// TODO: Add the stacks of the other threads 
        heap->garbageCollect(state->stack);
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
            state->global_arg1 = p;
            state->global_arg2 = proc;
            return (fn_ptr)&eval_user_procedure_call;
        } else if (proc->type() == SchemeObject::BUILT_IN_PROCEDURE) {
            state->global_arg1 = p;
            state->global_arg2 = proc;
            return (fn_ptr)&eval_built_in_procedure_call;
        } else if (proc->type() == SchemeObject::CONTINUATION) {
            state->global_arg1 = i_car(cdr);
            eval(state);
            proc->callContinuation(state->global_ret);
        } else if (proc->type() == SchemeObject::MACRO) {
            state->global_arg1 = proc;
            state->global_arg2 = cdr;
            return (fn_ptr)&eval_call_macro;
        } else if (proc->type() == SchemeObject::INTERNAL_PROCEDURE) {
            if (s == if_symbol) {
                state->global_arg1 = p;
                return (fn_ptr)&eval_if;
            } else if (s == quote_symbol) {
                state->global_ret = i_car(cdr);
                return NULL;
            } else if (s == define_symbol) {
                state->global_arg1 = p;
                return (fn_ptr)&eval_define;
            } else if (s == define_macro) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_define_macro;
            } else if (s == quasiquote_symbol) {
                state->global_arg1 = i_car(cdr);
                return (fn_ptr)&eval_quasiquote;
            } else if (s == lambda_symbol || s == lambda_symbol_short) {
        	    SchemeObject* formals = s_car(cdr);
                SchemeObject* body = s_cdr(cdr);
                state->global_arg1 = formals;
                state->global_arg2 = body;
                state->global_arg3 = unnamed_symbol;
                return (fn_ptr)&eval_lambda;
            } else if (s == let_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_let;
            } else if (s == do_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_do;
            } else if (s == letstar_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_letstar;
            } else if (s == letrec_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_letrec;
            } else if (s == apply_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_apply;
            } else if (s == set_e_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_set_e;
            } else if (s == begin_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_begin;
            } else if (s == cond_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_cond;
            } else if (s == case_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_case;
            } else if (s == and_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_and;
            } else if (s == or_symbol) {
                state->global_arg1 = cdr;
                return (fn_ptr)&eval_or;
            } else {
                throw scheme_exception(p->src_line(), L"Unknown internal procedure: " + proc->toString());	
            }            
        } else {
            throw scheme_exception(p->src_line(), L"Wrong type to apply : " + proc->toString());	
        }
    } else {
	    throw scheme_exception(s->src_line(), L"Unbound variable: " + s->toString());	
    }
    return NULL; // Never reached
}

//
// Evals a list of expressions and returns the last.
//
fn_ptr eval_sequence(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    if (p == S_EMPTY_LIST) {
        state->global_ret = S_UNSPECIFIED;
        return NULL;
    }
    if (i_cdr(p) == S_EMPTY_LIST) {
        // List has one element. Do a tail call.
        state->global_arg1 = i_car(p);
        return (fn_ptr)&eval;
    }
    state->stack.push_back(p);
    state->stack.push_back(state->global_envt);
    while (true) {
        if (i_cdr(p) == S_EMPTY_LIST) {
            // The tail call, let EVAL return to this' caller
            state->stack.pop_back();
            state->stack.pop_back();
            state->global_arg1 = i_car(p);
            return (fn_ptr)&eval;
        } else {
            state->global_arg1 = i_car(p);
            trampoline((fn_ptr)&eval, state);
            p = i_cdr(p);
        }
    }    
}

//
// Evals a list of expressions and the returns the list of results
//
fn_ptr eval_multi(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    state->stack.push_back(p);
    state->stack.push_back(state->global_envt);
    
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* result_tail = S_EMPTY_LIST;
    state->stack.push_back(result);
    SchemeObject** result_stack_ref = &(state->stack.back());
    
    while (p != S_EMPTY_LIST) {
        state->global_arg1 = i_car(p);
        SchemeObject* r;
        if (state->global_arg1->self_evaluating()) {
            r = state->global_arg1;
        } else {
            r = trampoline((fn_ptr)&eval, state);
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
    state->stack.pop_back();
    state->stack.pop_back();
    state->stack.pop_back();
    state->global_ret = result;
    return NULL;
}


fn_ptr eval_define(Interpreter::State* state) {
    SchemeObject* plist = state->global_arg1;
    SchemeObject* p = i_cdr(plist);
    SchemeObject* envt = state->global_envt;
    
    if (p == S_EMPTY_LIST || i_car(p) == S_EMPTY_LIST || i_cdr(p) == S_EMPTY_LIST) {
        throw scheme_exception(plist->src_line(), L"Too few arguments: define");
    }
    
    if (i_pair_p(i_car(p)) == S_TRUE) {
        // (define (func-name args...) body-forms...)
        SchemeObject* pa = i_car(p);
        SchemeObject* name = i_car(pa);
        if (i_symbol_p(name) == S_FALSE) {
                throw scheme_exception(plist->src_line(), L"Bad variable: define: " + name->toString());
        }
        SchemeObject* body = i_cdr(p);

        state->global_arg1 = i_cdr(pa);
        state->global_arg2 = body;
        state->global_arg3 = name;
        SchemeObject* proc = trampoline((fn_ptr)&eval_lambda, state);
        
        envt->defineBinding(name, proc);
    } else {
        // (define var value-expr)
        if (i_cddr(p) != S_EMPTY_LIST) {
            throw scheme_exception(plist->src_line(), L"Too many arguments: define");
        }
        
        SchemeObject* s = i_car(p);
        if (i_symbol_p(s) == S_FALSE) {
            throw scheme_exception(plist->src_line(), L"Bad variable: define: " + s->toString());
        }
        
        state->global_arg1 = i_car(i_cdr(p));
        SchemeObject* v = trampoline((fn_ptr)&eval, state);

        envt->defineBinding(s, v);
    }
    state->global_ret = S_UNSPECIFIED;
    return NULL;
}


fn_ptr eval_begin(Interpreter::State* state) {
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_apply(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    state->stack.push_back(p);
    
    if (s_procedure_p(s_car(p)) == S_TRUE) {
        state->stack.pop_back();
        state->global_arg1 = s_car(p);
        state->global_arg2 = s_car(s_cdr(p));
        return (fn_ptr)&eval_procedure_call;
    }
    
    state->global_arg1 = s_car(p);
    SchemeObject* proc = trampoline((fn_ptr)&eval, state);
    assert_arg_type(L"apply", 1, s_procedure_p, proc);
    
    state->global_arg1 = s_cdr(p);
    SchemeObject* args = trampoline((fn_ptr)&eval_multi, state);

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
                throw scheme_exception(L"Illegal argument");
            }
        } else {
            if (collected == S_EMPTY_LIST) {
                collected = s_cons(arg, S_EMPTY_LIST);
                prev = collected;
                state->stack.push_back(collected);
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
            state->stack.pop_back();
        }
        state->stack.pop_back();
        // Hack to handle the test (apply apply `(,+ ,(list 1 2)))
        state->global_arg1 = i_cons(s_car(p), collected);
        return (fn_ptr)&eval_list;
    }

    if (collected != S_EMPTY_LIST) {
        state->stack.pop_back();
    }
    state->stack.pop_back();
    state->global_arg1 = proc;
    state->global_arg2 = collected;
    return (fn_ptr)&eval_procedure_call;
}

//
// ((form) args)		    
// where form is an expression that should evaluate to a procedure that we execute
//
fn_ptr eval_combo(Interpreter::State* state) {
    SchemeObject* s = state->global_arg1;
    state->stack.push_back(s);
    
    state->global_arg1 = s_car(s);
    SchemeObject* proc = trampoline((fn_ptr)&eval, state);
    
    if (i_procedure_p(proc) == S_FALSE) {
	throw scheme_exception(L"Wrong type to apply: " + s->toString() + L" does not resolve to a procedure.");
    }
    
    state->stack.push_back(proc);
    
    state->global_arg1 = s_cdr(s);
    SchemeObject* args = trampoline((fn_ptr)&eval_multi, state);
    
    state->stack.pop_back();
    state->stack.pop_back();
    
    state->global_arg1 = proc;
    state->global_arg2 = args;
    return (fn_ptr)&eval_procedure_call;
}

//
// (if condition true-form false-form) 
// where false-form is optional.
//
fn_ptr eval_if(Interpreter::State* state) {
    SchemeObject* spair = state->global_arg1;        
    SchemeObject* p = i_cdr(spair);

    if (p == S_EMPTY_LIST) {
        throw scheme_exception(spair->src_line(), L"Condition missing in: if");
    } else if (i_cdr(p) == S_EMPTY_LIST) {
        throw scheme_exception(spair->src_line(), L"Consequent missing in: if");
    }
    
    state->stack.push_back(p);
    state->global_arg1 = i_car(p);
    bool condition = scm2bool(trampoline((fn_ptr)&eval, state));
    state->stack.pop_back();

    if (i_cdr(i_cdr(p)) != S_EMPTY_LIST && i_cdr(i_cdr(i_cdr(p))) != S_EMPTY_LIST) {
        throw scheme_exception(spair->src_line(), L"Too many expressions in if-statement.");
    }

    if (condition) {
        // Evaluate and return true case
        SchemeObject* true_case = i_car(i_cdr(p));
        state->global_arg1 = true_case;
        return (fn_ptr)&eval;
    } else if (i_cdr(i_cdr(p)) != S_EMPTY_LIST) {
        // Evaluate and return false case
    	SchemeObject* false_case = i_car(i_cdr(i_cdr(p)));
        state->global_arg1 = false_case;
        return (fn_ptr)&eval;
    } else {
        state->global_ret = S_UNSPECIFIED;
        return NULL;
    }    
}

fn_ptr eval_and(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    state->stack.push_back(p);
    
    SchemeObject *cur, *next, *result = S_TRUE;

    while (p != S_EMPTY_LIST) {
        cur = i_car(p);
        next = i_cdr(p);
        if (next == S_EMPTY_LIST) {
            // Tail call
            state->stack.pop_back();
            state->global_arg1 = cur;
            return (fn_ptr)&eval;
        } else {
            state->global_arg1 = cur;
            result = trampoline((fn_ptr)&eval, state);
            if (!scm2bool(result)) {
                state->stack.pop_back();
                state->global_ret = result;
                return NULL;
            }
            p = next;
        } 
    }
    state->stack.pop_back();
    state->global_ret = result;
    return NULL;
}

fn_ptr eval_or(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    state->stack.push_back(p);
    state->stack.push_back(state->global_envt);
    
    SchemeObject* result = S_FALSE;
    while (p != S_EMPTY_LIST) {
        if (s_cdr(p) == S_EMPTY_LIST) {
            // Tail call
            state->stack.pop_back();
            state->stack.pop_back();
            state->global_arg1 = s_car(p);
            return (fn_ptr)&eval;
        } else {
            state->global_arg1 = s_car(p);
            result = trampoline((fn_ptr)&eval, state);

            if (scm2bool(result)) {
                state->stack.pop_back();
                state->stack.pop_back();
                state->global_ret = result;
                return NULL;
            }
            p = s_cdr(p);
        }
    }
    state->stack.pop_back();
    state->stack.pop_back();
    state->global_ret = result;
    return NULL;
}

SchemeObject* eval_quasiquote_recursive(SchemeObject* o, int level, Interpreter::State* state);

SchemeObject* eval_unquote_recursive(SchemeObject* o, int level, Interpreter::State* state) {
    SchemeObject* result;
    state->stack.push_back(o);
    if (level == 0) {
        state->global_arg1 = s_car(s_cdr(o));
        result = trampoline((fn_ptr)&eval, state);
    } else {
        result = s_cons(unquote_symbol, eval_quasiquote_recursive(s_cdr(o), level-1, state));
    }
    state->stack.pop_back();
    return result;
}

SchemeObject* eval_quasiquote_recursive(SchemeObject* o, int level, Interpreter::State* state) {
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
            result = eval_unquote_recursive(p,level,state);
        } else if (car_p == unquote_splicing_symbol && s_vector_p(o) == S_FALSE) {
            result = eval_unquote_recursive(p,level, state);
            if (s_list_p(result) == S_FALSE) {
                throw scheme_exception(L"unquote-splicing must result in a list");
            }
        } else if (car_p == quasiquote_symbol && level == 0) {
            result = s_cons(quasiquote_symbol, eval_quasiquote_recursive(s_cdr(o), level + 1, state));
        } else {
            // p is a list or vector that we recurse into
            result = S_EMPTY_LIST;
            SchemeObject* prev = S_EMPTY_LIST;
            while(true) {
                if (i_pair_p(p) == S_TRUE) {
                    
                    if (s_car(p) == unquote_symbol) {
                        // Handle when final cdr of unproper list is a unquote
                        state->stack.push_back(result);
                        s_set_cdr_e(prev,eval_unquote_recursive(p, level, state));
                        state->stack.pop_back();
                        break;
                    }
                    
                    state->stack.push_back(p);
                    state->stack.push_back(result);
                    SchemeObject* r = eval_quasiquote_recursive(s_car(p),level, state);
                    state->stack.pop_back();
                    state->stack.pop_back();

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
                    state->stack.push_back(result);
                    SchemeObject* r = eval_quasiquote_recursive(p, level, state);
                    state->stack.pop_back();
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

fn_ptr eval_quasiquote(Interpreter::State* state) {
    state->global_ret = eval_quasiquote_recursive(state->global_arg1, 0, state);
    return NULL;
}

fn_ptr eval_user_procedure_call(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;        
    SchemeObject* proc = state->global_arg2;
    SchemeObject* args_to_eval = i_cdr(p);
    
    assert(proc != NULL);

    SchemeObject* new_envt = SchemeObject::createEnvironment(proc->s_envt());
    SchemeObject* formals = proc->s_formals();

    state->stack.push_back(proc);
    state->stack.push_back(p);
    state->stack.push_back(new_envt);
    
    while (i_pair_p(formals) == S_TRUE) {
        if (args_to_eval == S_EMPTY_LIST) {
            throw scheme_exception(p->src_line(), L"Too few argument given in call to: "+proc->nameAsString());
        }
        
        state->global_arg1 = i_car(args_to_eval);
        SchemeObject* arg = trampoline((fn_ptr)&eval, state);
        
        new_envt->defineBinding(i_car(formals), arg);
        args_to_eval = i_cdr(args_to_eval);
        formals = i_cdr(formals);
    }
    if (formals != S_EMPTY_LIST) {
        // Rest argument    
        state->global_arg1 = args_to_eval;
        SchemeObject* args = trampoline((fn_ptr)&eval_multi, state);
        new_envt->defineBinding(formals, args);
    } else if (args_to_eval != S_EMPTY_LIST) {
        throw scheme_exception(p->src_line(), L"Too many argument given in call to: "+proc->nameAsString());
    }

    state->stack.pop_back();
    state->stack.pop_back();
    state->stack.pop_back();
    
    SchemeObject* body = proc->s_body();
    state->global_envt = new_envt;
    if (i_cdr(body) == S_EMPTY_LIST && i_pair_p(i_car(body)) == S_TRUE) {
        // If body is a single form, which is the case most times, then call eval-list directly on it.
        state->global_arg1 = i_car(body);
        return (fn_ptr)&eval_list;
    } else {
        state->global_arg1 = body;
        return (fn_ptr)&eval_sequence;
    }
}

fn_ptr eval_built_in_procedure_call(Interpreter::State* state) 
{
    SchemeObject* p = state->global_arg1;        
    SchemeObject* proc = state->global_arg2;
    SchemeObject* args = i_cdr(p);
    
    assert(proc != NULL);

    state->stack.push_back(proc);
    state->stack.push_back(p);

    SchemeObject* result = S_UNSPECIFIED;

    // Built-in function
    int req = proc->req();
    int opt = proc->opt();
    bool rst = proc->rest();
    
    for(int i = 0; i < req; i++) {
        if (args == S_EMPTY_LIST) {
            throw scheme_exception(p->src_line(), L"Too few argument given in call to: "+proc->nameAsString());
        }
        state->global_arg1 = i_car(args);
        SchemeObject* arg = trampoline((fn_ptr)&eval, state);
        state->stack.push_back(arg);
        args = i_cdr(args);
    }
    for(int i = 0; i < opt; i++) {
        if (args != S_EMPTY_LIST) {
            state->global_arg1 = i_car(args);
            SchemeObject* arg = trampoline((fn_ptr)&eval, state);
            state->stack.push_back(arg);
            args = i_cdr(args);
        } else {
            state->stack.push_back(S_UNSPECIFIED);
        }
    }
    
    int num = req + opt;

    if (rst) {
        while (args != S_EMPTY_LIST) {
            state->global_arg1 = i_car(args);
            SchemeObject* arg = trampoline((fn_ptr)&eval, state);
            state->stack.push_back(arg);
            args = i_cdr(args);
            num++;
        }    
    } else {
        if (args != S_EMPTY_LIST) {
            throw scheme_exception(p->src_line(), L"Too many argument given in call to: "+proc->nameAsString());
        }    
    }
    
    try {
        SchemeStack::iterator stack_iter = state->stack.end() - num;

        if (rst) {
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
                default:  throw scheme_exception(p->src_line(), L"Doesn't support that many args to a built-in function."); 
            }
        }
    } catch (scheme_exception e) {
        //string s = "In call to procedure " + proc->nameAsString() + ": " + e.toString();
        throw scheme_exception(p->src_line(), e.toString());
    }
    for(int i = 0; i < num; i++) {
        state->stack.pop_back();
    }
    state->stack.pop_back();
    state->stack.pop_back();
    state->global_ret = result;
    return NULL;    
}

fn_ptr eval_procedure_call(Interpreter::State* state) {
    SchemeObject* proc = state->global_arg1;
    SchemeObject* args = state->global_arg2;
    
    assert(proc != NULL);
    
    state->stack.push_back(proc);
    state->stack.push_back(args);
    
    SchemeObject* result = S_UNSPECIFIED;
    
    if (proc->type() == SchemeObject::BUILT_IN_PROCEDURE) {
        // Built-in function
        int req = proc->req();
        int opt = proc->opt();
        bool rst = proc->rest();

        for(int i = 0; i < req; i++) {
            if (args == S_EMPTY_LIST) {
                throw scheme_exception(L"Too few argument given in call to "+proc->nameAsString());
            }
            state->stack.push_back(i_car(args));
            args = i_cdr(args);
        }
        for(int i = 0; i < opt; i++) {
            if (args != S_EMPTY_LIST) {
                state->stack.push_back(i_car(args));
                args = i_cdr(args);
            } else {
                state->stack.push_back(S_UNSPECIFIED);
            }
        }

        int num = req + opt;
        
        if (!rst && args != S_EMPTY_LIST) {
            throw scheme_exception(L"Too many argument given in call to "+proc->nameAsString());
        }
        if (rst) {
            while (args != S_EMPTY_LIST) {
                 state->stack.push_back(i_car(args));
                 args = i_cdr(args);
                 num++;
            }        
        }
        try {
            SchemeStack::iterator stack_iter = state->stack.end() - num;
            
            if (rst) {
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
                    default:  throw scheme_exception(L"Doesn't support that many args to a built-in function."); 
                }
            }
        } catch (scheme_exception e) {
            wstring s = L"In call to procedure " + proc->nameAsString() + L": " + e.toString();
            throw scheme_exception(s);
        }
        for(int i = 0; i < num; i++) {
            state->stack.pop_back();
        }
        state->stack.pop_back();
        state->stack.pop_back();
        state->global_ret = result;
        return NULL;
    } else {
        // User function
        SchemeObject* new_envt = SchemeObject::createEnvironment(proc->s_envt());
        SchemeObject* formals = proc->s_formals();

        while (i_pair_p(formals) == S_TRUE) {
            if (args == S_EMPTY_LIST) {
                throw scheme_exception(L"Too few argument given in call to "+proc->nameAsString());
            }
            new_envt->defineBinding(i_car(formals), i_car(args));
            args = i_cdr(args);
            formals = i_cdr(formals);
        }
        if (formals != S_EMPTY_LIST) {
            new_envt->defineBinding(formals, args);
        } else if (args != S_EMPTY_LIST) {
            throw scheme_exception(L"Too many argument given in call to "+proc->nameAsString());
        }

        state->global_envt = new_envt;
        state->global_arg1 = proc->s_body();
        state->stack.pop_back();
        state->stack.pop_back();
        return (fn_ptr)&eval_sequence;
    }    
}

fn_ptr eval_lambda(Interpreter::State* state) {
    // TODO: Memoize eval_lambda, ie. cache the result.
    // TODO: Check that all formals are symbols
    SchemeObject* formals = state->global_arg1;
    SchemeObject* body = state->global_arg2;
    SchemeObject* name = state->global_arg3;
    SchemeObject* envt = state->global_envt;

    state->global_ret = SchemeObject::createUserProcedure(name, envt, formals, body);
    return NULL;
}

fn_ptr eval_set_e(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    SchemeObject* envt = state->global_envt;

    if (p == S_EMPTY_LIST || i_cdr(p) == S_EMPTY_LIST || i_cddr(p) != S_EMPTY_LIST) {
        throw scheme_exception(L"Missing or extra arguments to set!");
    }
    SchemeObject* symbol = i_car(p);
    if (i_symbol_p(symbol) == S_FALSE) {
        throw scheme_exception(L"Wrong type argument in position 1.");
    }

    state->global_arg1 = i_cadr(p);
    SchemeObject* value = trampoline((fn_ptr)&eval, state);

    envt->setBinding(symbol, value);

    state->global_ret = S_UNSPECIFIED;
    return NULL;
}

fn_ptr eval_cond(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;

    while (i_null_p(p) == S_FALSE) {
        SchemeObject* clause = s_car(p);
        if (i_pair_p(clause) == S_FALSE) {
            throw scheme_exception(L"Invalid clause");
        }
        SchemeObject* test_expr = s_car(clause);
        if (test_expr == else_symbol) {
            // Handle (else <expressions> ...)
            if (i_null_p(s_cdr(p)) == S_FALSE) {
                throw scheme_exception(L"else-clause must be last");
            }
            state->global_arg1 = s_cdr(clause);
            return (fn_ptr)&eval_sequence;
        }
        
        // Eval test_expr
        state->global_arg1 = test_expr;
        SchemeObject* test = trampoline((fn_ptr)&eval, state);
        
        if (scm2bool(test)) {
            if (s_cdr(clause) == S_EMPTY_LIST) {
                state->global_ret = test;
                return NULL;
            } else if (s_car(s_cdr(clause)) == ergo_symbol) {
                // Handle (<test> => <expression>)
                state->global_arg1 = s_car(s_cdr(s_cdr(clause)));
                SchemeObject* proc = trampoline((fn_ptr)&eval, state);

                state->global_arg1 = proc;
                state->global_arg2 = s_cons(test,S_EMPTY_LIST);
                return (fn_ptr)&eval_procedure_call;
            } else {
                state->global_arg1 = s_cdr(clause);
                return (fn_ptr)&eval_sequence;
            }
        }
        p = s_cdr(p);
    }
    state->global_ret = S_UNSPECIFIED;
    return NULL;
}

fn_ptr eval_case(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    state->stack.push_back(p);

    // Eval key       
    state->global_arg1 = s_car(p);
    SchemeObject* key = trampoline((fn_ptr)&eval, state);
    
    p = s_cdr(p);

    while (i_null_p(p) == S_FALSE) {
        SchemeObject* clause = s_car(p);
        if (i_pair_p(clause) == S_FALSE) {
            throw scheme_exception(L"Invalid clause");
        }
        SchemeObject* clause_car = s_car(clause);
        if (clause_car == else_symbol) {
            // Handle (else <expressions> ...)
            if (i_null_p(s_cdr(p)) == S_FALSE) {
                throw scheme_exception(L"else-clause must be last");
            }
            state->stack.pop_back();
            state->global_arg1 = s_cdr(clause);
            return (fn_ptr)&eval_sequence;
        } else if (i_pair_p(clause_car) == S_TRUE) {
            if (s_memv(key,clause_car) != S_FALSE) {
                state->stack.pop_back();
                state->global_arg1 = s_cdr(clause);
                return (fn_ptr)&eval_sequence;
            }
        } else {
            throw scheme_exception(L"Invalid clause in case-statement");
        }

        p = s_cdr(p);
    }
    state->stack.pop_back();
    state->global_ret = S_UNSPECIFIED;
    return NULL;    
}

fn_ptr eval_let(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    SchemeObject* envt = state->global_envt;
    
    if (i_null_p(p) == S_TRUE) {
        throw scheme_exception(L"Bad body in let");
    }
    
    SchemeObject* first_arg = i_car(p);

    if (s_symbol_p(first_arg) == S_TRUE) {
        return (fn_ptr)&eval_named_let;
    }
    
    if (i_pair_p(first_arg) == S_FALSE && i_null_p(first_arg) == S_FALSE) {
        throw scheme_exception(L"Bad body in let");
    }
    
    // Build new bindings
    SchemeObject* new_bindings = SchemeObject::createEnvironment(envt);
    SchemeObject* binding_pairs = first_arg;
    
    state->stack.push_back(new_bindings);

    while (i_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        
        state->global_arg1 = s_car(s_cdr(s_car(binding_pairs)));
        SchemeObject* val = trampoline((fn_ptr)&eval, state);

        new_bindings->defineBinding(s_car(s_car(binding_pairs)), val);
        binding_pairs = s_cdr(binding_pairs);
    }
    state->stack.pop_back();
    
    state->global_envt = new_bindings;
    state->global_arg1 = s_cdr(p);
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_named_let(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    SchemeObject* envt = state->global_envt;
    
    SchemeObject* name = s_car(p);
    p = s_cdr(p);
    
    if (i_pair_p(s_car(p)) == S_FALSE && i_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception(L"Bad formals in let");
    }
    
    // Extract formals and collect args for a lambda
    SchemeObject* binding_pairs = s_car(p);
    SchemeAppendableList formals;
    SchemeAppendableList args;

    while (i_null_p(binding_pairs) == S_FALSE) {
        SchemeObject* binding_pair = s_car(binding_pairs);
        SchemeObject* formal = s_car(binding_pair);
        SchemeObject* arg = s_car(s_cdr(binding_pair));
        
	formals.add(formal);
	args.add(arg);

        binding_pairs = s_cdr(binding_pairs);
    }
    
    SchemeObject* new_envt = SchemeObject::createEnvironment(envt);
    SchemeObject* lambda = SchemeObject::createUserProcedure(name, new_envt, formals.list, s_cdr(p));
    new_envt->defineBinding(name, lambda);
    
    
    state->global_arg1 = i_cons(S_FALSE,args.list);
    state->global_arg2 = lambda;
    return (fn_ptr)&eval_user_procedure_call;
}

fn_ptr eval_letstar(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    SchemeObject* envt = state->global_envt;
    
    if (i_null_p(p) == S_TRUE) {
        throw scheme_exception(L"Bad body in let*");
    }
    
    if (i_pair_p(s_car(p)) == S_FALSE && i_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception(L"Bad formals in let*: " + s_car(p)->toString());
    }
    
    if (i_null_p(s_cdr(p)) == S_TRUE) {
        throw scheme_exception(L"Missing body in let*");
    }

    // Build new bindings
    SchemeObject* new_bindings = SchemeObject::createEnvironment(envt);
    SchemeObject* binding_pairs = s_car(p);

    while (i_null_p(binding_pairs) == S_FALSE) {
        // Eval binding value
        state->global_arg1 = s_cadar(binding_pairs);
        state->global_envt = new_bindings;
        SchemeObject* val = trampoline((fn_ptr)&eval, state);
        
	SchemeObject* sym = s_car(s_car(binding_pairs));
	if (s_symbol_p(sym) == S_FALSE) {
	    throw scheme_exception(L"Bad variable in let*: " + s_car(s_car(binding_pairs))->toString());
	}
        new_bindings->defineBinding(sym, val);
        binding_pairs = s_cdr(binding_pairs);
    }
    
    state->global_envt = new_bindings;
    state->global_arg1 = s_cdr(p);
    return (fn_ptr)&eval_sequence;
}

fn_ptr eval_letrec(Interpreter::State* state) {
    return (fn_ptr)&eval_letstar;
}

fn_ptr eval_define_macro(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    SchemeObject* envt = state->global_envt;

    SchemeObject* formals = s_car(p);
    SchemeObject* body = s_cdr(p);
    SchemeObject* name = s_car(formals);
    formals = s_cdr(formals);
    
    if (i_symbol_p(name) == S_FALSE) {
        throw scheme_exception(L"Invalid macro-name in definition: " + name->toString());
    }

    SchemeObject* macro = SchemeObject::createMacro(name, envt, formals, body);
    envt->defineBinding(name, macro);
    
    state->global_ret = S_UNSPECIFIED;
    return (fn_ptr) NULL;
}

fn_ptr eval_call_macro(Interpreter::State* state) {
    SchemeObject* proc = state->global_arg1;
    SchemeObject* args = state->global_arg2;
    SchemeObject* envt = state->global_envt;
    
    // Build new environment
    SchemeObject* new_envt = SchemeObject::createEnvironment(proc->s_envt());
    SchemeObject* formals = proc->s_formals();
    
    while (i_pair_p(formals) == S_TRUE) {
        if (args == S_EMPTY_LIST) {
            throw scheme_exception(L"Too few argument given in call to macro " + proc->nameAsString());
        }
        new_envt->defineBinding(i_car(formals), i_car(args));
        formals = i_cdr(formals);
        args = i_cdr(args);
    }
    if (formals != S_EMPTY_LIST) {
        new_envt->defineBinding(formals, args);
    } else if (args != S_EMPTY_LIST) {
        throw scheme_exception(L"Too many argument given in call to macro "+proc->nameAsString());
    }
    
    // cout << "Body: " << proc->s_body->toString() << endl;
    // Transform body
    state->stack.push_back(envt);
    state->stack.push_back(proc);
    state->stack.push_back(new_envt);
    state->global_envt = new_envt;
    state->global_arg1 = proc->s_body();
    SchemeObject* transformed_body = trampoline((fn_ptr)&eval_sequence, state);
    //cout << "Transformed body: " << transformed_body->toString() << endl;
    state->stack.pop_back();
    state->stack.pop_back();
    state->stack.pop_back();
    
    // Eval transformed body
    state->global_envt = envt;
    state->global_arg1 = transformed_body;
    return (fn_ptr)&eval;    
}

fn_ptr eval_do(Interpreter::State* state) {
    SchemeObject* p = state->global_arg1;
    SchemeObject* envt = state->global_envt;

    if (i_pair_p(s_car(p)) == S_FALSE && i_null_p(s_car(p)) == S_FALSE) {
        throw scheme_exception(L"Bad body in do");
    }

    // Extract formals and collect evaluated args for a lambda
    SchemeObject* new_envt = SchemeObject::createEnvironment(envt);
    SchemeObject* binding_pairs = s_car(p);
    
    state->stack.push_back(p);
    state->stack.push_back(new_envt);

    SchemeObject* binding_pairs_ptr = binding_pairs;
    uint32_t bindings_num = 0;
    while (binding_pairs_ptr != S_EMPTY_LIST) {
        bindings_num++;    
        binding_pairs_ptr = i_cdr(binding_pairs_ptr);
    }
    
    SchemeObject* varnames[bindings_num];
    SchemeObject* steps[bindings_num];
    
    binding_pairs_ptr = binding_pairs;
    for(uint32_t i = 0; i < bindings_num; i++) {
        SchemeObject* binding = i_car(binding_pairs_ptr);

        if (i_pair_p(binding) == S_FALSE) {
            throw scheme_exception(L"Invalid binding in do-form");
        }

        // Binding symbol
        SchemeObject* varname = i_car(binding);
        if (i_symbol_p(varname) == S_FALSE) {
            throw scheme_exception(L"Invalid variable in do: " + varname->toString());
        }
        varnames[i] = varname;

        // Eval initial binding value
        if (i_cdr(binding) == S_EMPTY_LIST) {
            throw scheme_exception(L"In do: missing initial value for variable " + varname->toString());
        }
        state->global_arg1 = i_cadr(binding);
        SchemeObject* val = trampoline((fn_ptr)&eval, state);
        new_envt->defineBinding(varname, val);

        // Save step expression
        SchemeObject* step = i_cddr(binding);
        steps[i] = step == S_EMPTY_LIST ? varname : i_car(step);
        binding_pairs_ptr = i_cdr(binding_pairs_ptr);
    }
    
    SchemeObject* body = i_cdr(p);
    if (body == S_EMPTY_LIST || i_pair_p(i_car(body)) == S_FALSE) {
        throw scheme_exception(L"Missing exit clause in do");
    }

    state->global_envt = new_envt;
    
    while (true) {
        // Evaluate test
        state->global_arg1 = i_caar(body);
        SchemeObject* val = trampoline((fn_ptr)&eval, state);

        // Return if test is true
        if (scm2bool(val)) {
            state->stack.pop_back();
            state->stack.pop_back();
            SchemeObject* return_sequence = i_cdar(body);
            if (return_sequence == S_EMPTY_LIST) {
                state->global_ret = S_UNSPECIFIED;
                return NULL;
            } else {
                state->global_arg1 = return_sequence;
                return (fn_ptr)&eval_sequence;
            }
        }
        
        // Evaluate body
        state->global_arg1 = i_cdr(body);
        trampoline((fn_ptr)&eval_sequence, state);
        
        // Evaluate steps
        for(uint32_t i = 0; i < bindings_num; i++) {
             state->global_arg1 = steps[i];        
             SchemeObject* val = trampoline((fn_ptr)&eval, state);
             state->stack.push_back(val);
        }
        
        // Assign new step values
        for(uint32_t i = 0; i < bindings_num; i++) {
             SchemeObject* val = state->stack.back();
             state->stack.pop_back();   
             new_envt->setBinding(varnames[bindings_num-1-i], val);
        }
    }
    return NULL; /* Never reached */
}
