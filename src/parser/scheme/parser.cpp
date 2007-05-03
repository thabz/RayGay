#include "parser.h"
#include "scheme.h"
#include <vector>

Parser::Parser() {
    this->lexer = new Lexer();
}

SchemePair* Parser::parse(istream* is) {
    SchemePair* result = S_EMPTY_LIST;
    SchemePair* result_tail = S_EMPTY_LIST;
    SchemeObject* o;
    while ((o = read(is)) != NULL) {
	SchemePair* newcell = s_cons(o, S_EMPTY_LIST);
	if (result == S_EMPTY_LIST) {
	    result = newcell;
	    result_tail = newcell;
	} else {
	    i_set_cdr_e(result_tail, newcell);
	    result_tail = newcell;
	}
    }
    return result;
}

SchemeObject* Parser::read(istream* is) {
    SchemeObject* v;
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
           v = s_vector(read_list(is));
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
    SchemeObject* result_tail = S_EMPTY_LIST;
    Lexer::Token token;
    while(true) {
        token = lexer->nextToken(is);
        if (token == Lexer::CLOSE_PAREN) {
            return result;
        } else if (token == Lexer::PERIOD) {
            SchemeObject* cdr = read(is);
	    if (result == S_EMPTY_LIST) {
                throw scheme_exception("Parser: invalid pair");
	    }
	    i_set_cdr_e(result_tail, cdr);
            if (lexer->nextToken(is) != Lexer::CLOSE_PAREN) {
                throw scheme_exception("Parser: invalid pair");
            }
            return result;
        } else if (token == Lexer::END) {
            throw scheme_exception("Unexpected end of input");
        } else {
            lexer->putBack(token);
	    SchemeObject* newcell = s_cons(read(is), S_EMPTY_LIST);
	    if (result == S_EMPTY_LIST) {
		result = newcell;
		result_tail = newcell;
	    } else {
		i_set_cdr_e(result_tail, newcell);
		result_tail = newcell;
	    }
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
