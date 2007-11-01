
#ifndef SCHEME_PARSER_H
#define SCHEME_PARSER_H

#include "lexer.h"
#include "objects.h"
#include "scheme.h"

class Parser 
{
    public:
	Parser();
	SchemeObject* parse(wistream* is);
        SchemeObject* read(wistream* is);

    private:
        SchemeObject* read_list(wistream* is);    
        SchemeObject* read_quoted(wistream* is);    
        SchemeObject* read_unquoted(wistream* is);    
        SchemeObject* read_unquote_spliced(wistream* is);    
        SchemeObject* read_quasiquoted(wistream* is);    

        void decorateWithLineNumber(SchemeObject* obj, uint32_t linenumber);

        Lexer* lexer;
};

#endif

