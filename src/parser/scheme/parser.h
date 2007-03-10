
#ifndef SCHEME_PARSER_H
#define SCHEME_PARSER_H

#include "lexer.h"
#include "objects.h"

class Parser 
{
    public:
	    Parser(Lexer* lexer);
	    SchemePair parse();

    private:
        Lexer* lexer;    
	    // Store a parsetree here

};

#endif

