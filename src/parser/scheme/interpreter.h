
#ifndef SCHEME_INTERPRETER_H
#define SCHEME_INTERPRETER_H

#include "objects.h"
#include "scheme.h"

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
		SchemeObject* eval_symbol(BindingEnvironment*, SchemeSymbol* s);
        SchemeObject* eval_sequence(BindingEnvironment*, SchemePair*);
		
        // Evaluators for special forms
		SchemeObject* eval_define(BindingEnvironment*, SchemePair*);
		SchemeObject* eval_set_e(BindingEnvironment*, SchemePair*);
		SchemeObject* eval_if(BindingEnvironment*, SchemePair*);
		SchemeObject* eval_let(BindingEnvironment*, SchemePair*);
};

#endif
