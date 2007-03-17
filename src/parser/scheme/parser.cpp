#include "parser.h"
#include "scheme.h"
#include <vector>

Parser::Parser(Lexer* lexer) {
    this->lexer = lexer;
}

SchemePair* Parser::parse() {
    SchemePair* result = S_EMPTY_LIST;
	SchemeObject* o;
    while ((o = read_simple()) != NULL) {
		result = s_cons(NULL, o, result);
    }
	return s_reverse(NULL, result);
}

SchemeObject* Parser::read_simple() {
    Lexer::Token token = lexer->nextToken();
    switch(token) {
        case Lexer::NUMBER :
           return new SchemeNumber(lexer->getNumber());
        case Lexer::STRING :
           return new SchemeString(lexer->getString());
        case Lexer::BOOLEAN :
			return lexer->getBool() ? S_TRUE : S_FALSE;
        case Lexer::SYMBOL :
           return new SchemeSymbol(lexer->getString());
        case Lexer::OPEN_PAREN :
           return read_list();
        case Lexer::QUOTE_OPEN_PAREN :
           return read_quoted_list();
        case Lexer::END :
  		   return NULL;
        case Lexer::ERROR :
           throw scheme_exception("Unknown lexer error");
        default:
           throw scheme_exception("Unexpected token");
   	       return NULL;
    }    
}

SchemeObject* Parser::read_list() {
    SchemeObject* result = S_EMPTY_LIST;
    Lexer::Token token;
    while(true) {
		token = lexer->nextToken();
        if (token == Lexer::CLOSE_PAREN) {
            // FIXME: We're leaking the old list. Garbage collect?
            return s_reverse(NULL, result);
        } else if (token == Lexer::END) {
            throw scheme_exception("Unexpected end of input");
        } else {
			lexer->putBack(token);
            result = s_cons(NULL, read_simple(), result);
        }
    }
}

SchemeObject* Parser::read_quoted_list() {
    SchemeObject* list = s_cons(NULL, read_list(), S_EMPTY_LIST);
    return s_cons(NULL, new SchemeSymbol("quote"), list);
}