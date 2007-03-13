
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
		
        // Evaluators for special forms
		SchemeObject* eval_if(BindingEnvironment*,SchemePair*);
		
};

#endif
