
#ifndef SCHEME_INTERPRETER_H
#define SCHEME_INTERPRETER_H

#include "objects.h"

class Interpreter
{
    public:
	    Interpreter(SchemePair tree);
   	    SchemeObject interpret();
   	    
   	private:
        SchemePair parsetree;    
};

#endif
