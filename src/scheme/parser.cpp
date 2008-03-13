#include "parser.h"
#include "scheme.h"
#include <vector>
#include "r6rs-lib-bytevectors.h"

Parser::Parser(Scheme* scheme) {
    this->lexer = new Lexer(scheme);
    this->scheme = scheme;
}

SchemeObject* Parser::parse(SchemeObject* port) {
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* result_tail = S_EMPTY_LIST;
    SchemeObject* o;
    while ((o = read(port)) != NULL) {
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

SchemeObject* Parser::read(SchemeObject* port) {
    SchemeObject* result;
    Lexer::Token token = lexer->nextToken(port);
    Lexer::Token tmp_token;
    uint32_t cur_line = lexer->getCurline();
    
    switch(token) {
        case Lexer::NUMBER :
	        result = lexer->getNumber();
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
            decorateWithLineNumber(result, cur_line);           
            break;
        case Lexer::OPEN_PAREN :
        case Lexer::OPEN_BRACKET : 
            result = read_list(port);
            tmp_token = lexer->nextToken(port); 
            if (token == Lexer::OPEN_PAREN && tmp_token != Lexer::CLOSE_PAREN) {
                throw scheme_exception(cur_line, L"Unbalanced parentheses");
            } else if (token == Lexer::OPEN_BRACKET && tmp_token != Lexer::CLOSE_BRACKET) {
                throw scheme_exception(cur_line, L"Unbalanced brackets");
            } else if (result != S_EMPTY_LIST) {
                decorateWithLineNumber(result, cur_line);           
            }
            break;
        case Lexer::HASH_OPEN_PAREN :
            result = s_list_2_vector(scheme,read_list(port));
            if (lexer->nextToken(port) != Lexer::CLOSE_PAREN) {
                throw scheme_exception(cur_line, L"Unbalanced parentheses");
            }
            break;
        case Lexer::VU8_OPEN_PAREN :
            result = s_u8_list_2_bytevector(scheme,read_list(port));
            if (lexer->nextToken(port) != Lexer::CLOSE_PAREN) {
                throw scheme_exception(cur_line, L"Unbalanced parentheses");
            }
            break;
        case Lexer::QUOTE :
            result = read_quoted(port);
            break;
        case Lexer::BACKQUOTE :
            result = read_quasiquoted(port);
            break;
        case Lexer::COMMA :
            result = read_unquoted(port);
            break;
        case Lexer::COMMA_AT :
            result = read_unquote_spliced(port);
            break;
	    case Lexer::DATUM_COMMENT :
	        read(port);  // Ignore following datum
	        result = read(port);
    	    break;   
        case Lexer::END :
            result = NULL;
            break;
        case Lexer::ERROR :
            throw scheme_exception(cur_line, lexer->getError());
        default:
            throw scheme_exception(cur_line, L"Unexpected token");
    }
    if (result != NULL) {
        result->set_immutable(true);
    }
    return result; 
}

SchemeObject* Parser::read_list(SchemeObject* port) {
    SchemeObject* result = S_EMPTY_LIST;
    SchemeObject* result_tail = S_EMPTY_LIST;
    Lexer::Token token;
    while(true) {
        token = lexer->nextToken(port);
        if (token == Lexer::CLOSE_PAREN || token == Lexer::CLOSE_BRACKET) {
            lexer->putBack(token);
            return result;
        } else if (token == Lexer::PERIOD) {
            SchemeObject* cdr = read(port);
	        if (result == S_EMPTY_LIST) {
                throw scheme_exception(L"Parser: invalid pair");
	        }
	        i_set_cdr_e(result_tail, cdr);
            if (lexer->peek(port) != Lexer::CLOSE_PAREN) {
                throw scheme_exception(L"Parser: invalid pair");
            }
            return result;
        } else if (token == Lexer::END) {
            throw scheme_exception(L"Unexpected end of input. Unbalanced parentheses?");
        } else {
            lexer->putBack(token);
    	    SchemeObject* newcell = i_cons(read(port), S_EMPTY_LIST);
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

SchemeObject* Parser::read_quoted(SchemeObject* port) {
    SchemeObject* list = i_cons(read(port), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol(L"quote"), list);
}

SchemeObject* Parser::read_quasiquoted(SchemeObject* port) {
    SchemeObject* list = i_cons(read(port), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol(L"quasiquote"), list);
}

SchemeObject* Parser::read_unquoted(SchemeObject* port) {
    SchemeObject* list = i_cons(read(port), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol(L"unquote"), list);
}

SchemeObject* Parser::read_unquote_spliced(SchemeObject* port) {
    SchemeObject* list = i_cons(read(port), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol(L"unquote-splicing"), list);
}

// Decorate an object with sourcecode line number.
// We need this do be able to write sensible error messages.
// The object metadata only has 24 bits for the line number.
void Parser::decorateWithLineNumber(SchemeObject* obj, uint32_t linenumber) {
    if (linenumber >= 1 << 24) {
        cout << "WARNING: Limited debugging capabilities of large file." << endl;    
    } else {
        obj->set_src_line(linenumber);
    }
}

