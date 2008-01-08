
#ifndef SCHEME_INTERPRETER_H
#define SCHEME_INTERPRETER_H

#include "objects.h"
#include "scheme.h"
#include <csetjmp>
#include <vector>
#include <pthread.h>

#define INTERPRETER_MAX_STACK_SIZE 10000

class Interpreter
{
    public:
	    Interpreter();
   	    SchemeObject* interpret(SchemeObject* parsetree, SchemeObject* top_level_envt);
        SchemeObject* call_procedure_0(SchemeObject* procedure);
        SchemeObject* call_procedure_1(SchemeObject* procedure, SchemeObject* arg);
        SchemeObject* call_procedure_2(SchemeObject* procedure, SchemeObject* arg1, SchemeObject* arg2);
        SchemeObject* call_procedure_3(SchemeObject* procedure, SchemeObject* arg1, SchemeObject* arg2, SchemeObject* arg3);
        SchemeObject* call_procedure_n(SchemeObject* procedure, SchemeObject* args_list);
   	    
        struct State {
            SchemeObject* global_ret;
            SchemeObject* global_arg1;
            SchemeObject* global_arg2;
            SchemeObject* global_arg3;
            SchemeObject* global_envt;
            vector<SchemeObject*> stack;
        };

       State* getState();   
   private:
       pthread_key_t state_key;
};

typedef void*(*fn_ptr)(Interpreter::State*);

SchemeObject* trampoline(fn_ptr, Interpreter::State* state);
fn_ptr eval(Interpreter::State*);
fn_ptr eval_sequence(Interpreter::State*);
fn_ptr eval_multi(Interpreter::State*);
fn_ptr eval_define(Interpreter::State*);
fn_ptr eval_apply(Interpreter::State*);
fn_ptr eval_apply_real(Interpreter::State*);
fn_ptr eval_procedure_call(Interpreter::State*);
fn_ptr eval_user_procedure_call(Interpreter::State*);
fn_ptr eval_built_in_procedure_call(Interpreter::State*);
fn_ptr eval_let(Interpreter::State*);
fn_ptr eval_letstar(Interpreter::State*);
fn_ptr eval_letrec(Interpreter::State*);
fn_ptr eval_named_let(Interpreter::State*);
fn_ptr eval_cond(Interpreter::State*);
fn_ptr eval_case(Interpreter::State*);
fn_ptr eval_and(Interpreter::State*);
fn_ptr eval_or(Interpreter::State*);
fn_ptr eval_set_e(Interpreter::State*);
fn_ptr eval_combo(Interpreter::State*);
fn_ptr eval_if(Interpreter::State*);
fn_ptr eval_define_macro(Interpreter::State*);
fn_ptr eval_call_macro(Interpreter::State*);
fn_ptr eval_quasiquote(Interpreter::State*);
fn_ptr eval_lambda(Interpreter::State*);
fn_ptr eval_do(Interpreter::State*);
fn_ptr eval_begin(Interpreter::State*);
fn_ptr eval_list(Interpreter::State*);

#endif
