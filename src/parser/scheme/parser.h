
#ifndef SCHEME_PARSER_H
#define SCHEME_PARSER_H

#include "lexer.h"
#include "objects.h"
#include "scheme.h"

class Parser 
{
    public:
	    Parser(Lexer* lexer);
	    SchemePair* parse();

    private:
        SchemeObject* read_simple();
        SchemeObject* read_list();    
        SchemeObject* read_quoted_list();    

        Lexer* lexer;
};

#endif

