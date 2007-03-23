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
		result = s_cons(o, result);
    }
	return s_reverse(result);
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
           return SchemeSymbol::create(lexer->getString());
        case Lexer::OPEN_PAREN :
           return read_list();
        case Lexer::HASH_OPEN_PAREN :
           return s_vector(static_cast<SchemePair*>(read_list()));
        case Lexer::QUOTE :
           return read_quoted();
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
            return s_reverse(result);
        } else if (token == Lexer::PERIOD) {
            SchemeObject* cdr = read_simple();
            SchemePair* rr = s_reverse(result);
            SchemePair* r = rr;
            while(r->cdr != S_EMPTY_LIST) {
                r = r->cdrAsPair();
            }
            r->cdr = cdr;
            if (lexer->nextToken() != Lexer::CLOSE_PAREN) {
                throw scheme_exception("Invalid pair");
            }
            return rr;
        } else if (token == Lexer::END) {
            throw scheme_exception("Unexpected end of input");
        } else {
			lexer->putBack(token);
            result = s_cons(read_simple(), result);
        }
    }
}

SchemeObject* Parser::read_quoted() {
    SchemeObject* list = s_cons(read_simple(), S_EMPTY_LIST);
    return s_cons(SchemeSymbol::create("quote"), list);
}