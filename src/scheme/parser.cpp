#include "parser.h"
#include "scheme.h"
#include <vector>

Parser::Parser() {
    this->lexer = new Lexer();
}

SchemeObject* Parser::parse(istream* is) {
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* result_tail = S_EMPTY_LIST;
    SchemeObject* o;
    while ((o = read(is)) != NULL) {
    	SchemeObject* newtail = i_cons(o, S_EMPTY_LIST);
    	if (result == S_EMPTY_LIST) {
    	    result = newtail;
    	    result_tail = newtail;
    	} else {
    	    i_set_cdr_e(result_tail, newtail);
    	    result_tail = newtail;
    	}
    }
    return result;
}

SchemeObject* Parser::read(istream* is) {
    SchemeObject* result;
    Lexer::Token token = lexer->nextToken(is);
    switch(token) {
        case Lexer::NUMBER :
           result = SchemeObject::createNumber(lexer->getNumber());
           break;
        case Lexer::STRING :
           result = SchemeObject::createString(lexer->getString().c_str());
           break;
        case Lexer::BOOLEAN :
           result = lexer->getBool() ? S_TRUE : S_FALSE;
           break;
        case Lexer::CHAR :
           result = SchemeObject::createChar(lexer->getChar());
           break;
        case Lexer::SYMBOL :
           result = SchemeObject::createSymbol(lexer->getString().c_str());
           break;
        case Lexer::OPEN_PAREN :
           result = read_list(is);
           break;
        case Lexer::HASH_OPEN_PAREN :
           result = s_vector(read_list(is));
           break;
        case Lexer::QUOTE :
           result = read_quoted(is);
           break;
        case Lexer::BACKQUOTE :
           result = read_quasiquoted(is);
           break;
        case Lexer::COMMA :
           result = read_unquoted(is);
           break;
        case Lexer::COMMA_AT :
           result = read_unquote_spliced(is);
           break;
        case Lexer::END :
           result = NULL;
           break;
        case Lexer::ERROR :
           throw scheme_exception("Unknown lexer error");
        default:
           throw scheme_exception("Parser: unexpected token");
    }
    if (result != NULL) {
        result->set_immutable(true);
    }
    return result; 
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
    	    SchemeObject* newcell = i_cons(read(is), S_EMPTY_LIST);
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
    SchemeObject* list = i_cons(read(is), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol("quote"), list);
}

SchemeObject* Parser::read_quasiquoted(istream* is) {
    SchemeObject* list = i_cons(read(is), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol("quasiquote"), list);
}

SchemeObject* Parser::read_unquoted(istream* is) {
    SchemeObject* list = i_cons(read(is), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol("unquote"), list);
}

SchemeObject* Parser::read_unquote_spliced(istream* is) {
    SchemeObject* list = i_cons(read(is), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol("unquote-splicing"), list);
}
