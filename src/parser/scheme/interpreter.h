
#ifndef SCHEME_INTERPRETER_H
#define SCHEME_INTERPRETER_H

#include "objects.h"
#include "scheme.h"

class Interpreter
{
    public:
	    Interpreter(SchemeObject* tree, Scheme* scheme);
   	    SchemeObject* interpret();
   	    
   	private:
		Scheme* scheme;
        SchemeObject* parsetree;
		SchemeObject* eval(SchemeObject* s);
		SchemeObject* eval_list(SchemePair* s);
		SchemeNumber* eval_plus(SchemePair* s);
		SchemeNumber* eval_mult(SchemePair* s);
		SchemeObject* eval_if(SchemePair* s);
		
};

#endif
