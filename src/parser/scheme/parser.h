
#ifndef SCHEME_PARSER_H
#define SCHEME_PARSER_H

#include "lexer.h"
#include "objects.h"
#include "scheme.h"

class Parser 
{
    public:
	    Parser(Lexer* lexer, Scheme* scheme);
	    SchemeObject* parse();

    private:
        SchemeObject* read_simple();
        SchemeObject* read_list();    

        Lexer* lexer;
        Scheme* scheme;
};

#endif

