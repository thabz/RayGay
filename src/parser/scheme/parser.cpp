#include "parser.h"
#include "scheme.h"
#include <vector>

Parser::Parser() {
    this->lexer = new Lexer();
}

SchemePair* Parser::parse(istream* is) {
    SchemePair* result = S_EMPTY_LIST;
	SchemeObject* o;
    while ((o = read(is)) != NULL) {
		result = s_cons(o, result);
    }
	return s_reverse(result);
}

SchemeObject* Parser::read(istream* is) {
    Lexer::Token token = lexer->nextToken(is);
    switch(token) {
        case Lexer::NUMBER :
           return SchemeNumber::create(lexer->getNumber());
        case Lexer::STRING :
           return SchemeString::create(lexer->getString(),true);
        case Lexer::BOOLEAN :
			return lexer->getBool() ? S_TRUE : S_FALSE;
        case Lexer::CHAR :
			return SchemeChar::create(lexer->getChar());
        case Lexer::SYMBOL :
           return SchemeSymbol::create(lexer->getString());
        case Lexer::OPEN_PAREN :
           return read_list(is);
        case Lexer::HASH_OPEN_PAREN :
           SchemeObject* v = s_vector(read_list(is));
           v->immutable = true;
           return v;
        case Lexer::QUOTE :
           return read_quoted(is);
        case Lexer::BACKQUOTE :
           return read_quasiquoted(is);
        case Lexer::COMMA :
           return read_unquoted(is);
        case Lexer::COMMA_AT :
           return read_unquote_spliced(is);
        case Lexer::END :
  		   return NULL;
        case Lexer::ERROR :
           throw scheme_exception("Unknown lexer error");
        default:
           throw scheme_exception("Parser: unexpected token");
   	       return NULL;
    } 
}

SchemeObject* Parser::read_list(istream* is) {
    SchemeObject* result = S_EMPTY_LIST;
    Lexer::Token token;
    while(true) {
		token = lexer->nextToken(is);
        if (token == Lexer::CLOSE_PAREN) {
            // FIXME: We're leaking the old list. Garbage collect?
            return s_reverse(result);
        } else if (token == Lexer::PERIOD) {
            SchemeObject* cdr = read(is);
            SchemePair* rr = s_reverse(result);
            SchemePair* r = rr;
            while(r->cdr != S_EMPTY_LIST) {
                r = r->cdrAsPair();
            }
            r->cdr = cdr;
            if (lexer->nextToken(is) != Lexer::CLOSE_PAREN) {
                throw scheme_exception("Invalid pair");
            }
            return rr;
        } else if (token == Lexer::END) {
            throw scheme_exception("Unexpected end of input");
        } else {
			lexer->putBack(token);
            result = s_cons(read(is), result);
        }
    }
}

SchemeObject* Parser::read_quoted(istream* is) {
    SchemeObject* list = s_cons(read(is), S_EMPTY_LIST);
    return s_cons(SchemeSymbol::create("quote"), list);
}

SchemeObject* Parser::read_quasiquoted(istream* is) {
    SchemeObject* list = s_cons(read(is), S_EMPTY_LIST);
    return s_cons(SchemeSymbol::create("quasiquote"), list);
}

SchemeObject* Parser::read_unquoted(istream* is) {
    SchemeObject* list = s_cons(read(is), S_EMPTY_LIST);
    return s_cons(SchemeSymbol::create("unquote"), list);
}

SchemeObject* Parser::read_unquote_spliced(istream* is) {
    SchemeObject* list = s_cons(read(is), S_EMPTY_LIST);
    return s_cons(SchemeSymbol::create("unquote-splicing"), list);
}
