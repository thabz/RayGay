
#ifndef SCHEME_INTERPRETER_H
#define SCHEME_INTERPRETER_H

#include "objects.h"
#include "scheme.h"
#include <setjmp.h>
#include <vector>

class StackEntry {
    public:
        enum Type {
            OBJECT,
            JMP_BUF,
            ENVT
        };
        SchemeObject* s_object;
        bool tail_call;
        ::jmp_buf jmpbuf;
        BindingEnvironment* envt;
        StackEntry::Type type;
};

class Stack {
    public:
        Stack();
        
        SchemeObject* popSchemeObject();
        SchemePair* popSchemePair();
        BindingEnvironment* popBindingEnvironment();
        void pop();
    
        void push(SchemeObject*);
        void push(BindingEnvironment* envt);
        jmp_buf* push_jump_pos();
        
        void return_jump(SchemeObject* return_value);
        
    private:
        std::vector<StackEntry> stk;
};


class Interpreter
{
    public:
	    Interpreter(SchemePair* parsetree, BindingEnvironment* top_level);
   	    SchemeObject* interpret();
   	    
   	private:
		BindingEnvironment* top_level_bindings;
        SchemePair* parsetree;

		SchemeObject* eval(BindingEnvironment*, SchemeObject* s);
		SchemeObject* eval_list(BindingEnvironment*, SchemePair* s);
		SchemeObject* eval_combo(BindingEnvironment*, SchemePair* s);
		SchemeObject* eval_symbol(BindingEnvironment*, SchemeSymbol* s);
        SchemeObject* eval_sequence(BindingEnvironment*, SchemePair*);
        SchemePair* eval_multi(BindingEnvironment*, SchemePair*);
		SchemeObject* eval_procedure_call(BindingEnvironment*, SchemeProcedure* f, SchemePair* args);
		
        // Evaluators for special forms
		SchemeObject* eval_define(BindingEnvironment*, SchemePair*);
		SchemeObject* eval_set_e(BindingEnvironment*, SchemePair*);
		SchemeObject* eval_if(BindingEnvironment*, SchemePair*);
		SchemeObject* eval_quote(BindingEnvironment*, SchemePair*);
		SchemeObject* eval_let(BindingEnvironment*, SchemePair*);
		SchemeProcedure* eval_lambda(BindingEnvironment* envt, SchemeObject* formals, SchemePair* body);
};

SchemeObject* eval2(BindingEnvironment* envt_orig, SchemeObject* seq_orig);


#endif
