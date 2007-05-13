
#ifndef SCHEME_PARSER_H
#define SCHEME_PARSER_H

#include "lexer.h"
#include "objects.h"
#include "scheme.h"

class Parser 
{
    public:
	    Parser();
	    SchemeObject* parse(istream* is);
        SchemeObject* read(istream* is);

    private:
        SchemeObject* read_list(istream* is);    
        SchemeObject* read_quoted(istream* is);    
        SchemeObject* read_unquoted(istream* is);    
        SchemeObject* read_unquote_spliced(istream* is);    
        SchemeObject* read_quasiquoted(istream* is);    

        Lexer* lexer;
};

#endif

