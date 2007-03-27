
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
	int size();
        
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
};

SchemeObject* eval(BindingEnvironment* envt_orig, SchemeObject* seq_orig);


#endif
