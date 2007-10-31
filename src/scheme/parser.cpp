#include "parser.h"
#include "scheme.h"
#include <vector>

Parser::Parser() {
    this->lexer = new Lexer();
}

SchemeObject* Parser::parse(wistream* is) {
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

SchemeObject* Parser::read(wistream* is) {
    SchemeObject* result;
    Lexer::Token token = lexer->nextToken(is);
    uint32_t cur_line = lexer->getCurline();
    
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
           decorateWithLineNumber(result, cur_line);           
           break;
        case Lexer::OPEN_PAREN :
           result = read_list(is);
           if (result != S_EMPTY_LIST) {
               decorateWithLineNumber(result, cur_line);           
           }
           break;
        case Lexer::HASH_OPEN_PAREN :
           result = s_list_2_vector(read_list(is));
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
	case Lexer::DATUM_COMMENT :
	   read(is);  // Ignore following datum
	   result = read(is);
    	   break;   
        case Lexer::END :
           result = NULL;
           break;
        case Lexer::ERROR :
           throw scheme_exception(cur_line, L"Unknown lexer error");
        default:
           throw scheme_exception(cur_line, L"Unexpected token");
    }
    if (result != NULL) {
        result->set_immutable(true);
    }
    return result; 
}

SchemeObject* Parser::read_list(wistream* is) {
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
                throw scheme_exception(L"Parser: invalid pair");
	    }
	    i_set_cdr_e(result_tail, cdr);
            if (lexer->nextToken(is) != Lexer::CLOSE_PAREN) {
                throw scheme_exception(L"Parser: invalid pair");
            }
            return result;
        } else if (token == Lexer::END) {
            throw scheme_exception(L"Unexpected end of input. Unbalanced parentheses?");
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

SchemeObject* Parser::read_quoted(wistream* is) {
    SchemeObject* list = i_cons(read(is), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol(L"quote"), list);
}

SchemeObject* Parser::read_quasiquoted(wistream* is) {
    SchemeObject* list = i_cons(read(is), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol(L"quasiquote"), list);
}

SchemeObject* Parser::read_unquoted(wistream* is) {
    SchemeObject* list = i_cons(read(is), S_EMPTY_LIST);
    return i_cons(SchemeObject::createSymbol(L"unquote"), list);
}

SchemeObject* Parser::read_unquote_spliced(wistream* is) {
    SchemeObject* list = i_cons(read(is), S_EMPTY_LIST);
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

int extractDigit(wchar_t c, uint32_t radix) {
    int result = -1;
    result = c - L'0';
    if (radix == 16) {
	if (result < 0 || result > 9) {
	    result = 10 + (tolower(c) - 'a');
	}
    }
    
    // Check result
    if (result >= 0 && result < int(radix)) {
	return result;
    } else {
	return -1;
    }
}

wchar_t digitToChar(int d, uint32_t radix) {
    if (radix == 16 && d > 9) {
	return L'a' + d;
    } else {
	return L'0' + d;
    }
}

wstring extractDigits(wstring s, size_t offset, uint32_t radix) {
    wstring result;
    uint32_t i = 0;
    while (offset + i < s.size()) {
	wchar_t digit = s[offset+i];
	int d = extractDigit(digit,radix);
	if (d == -1) {
	    return result;
	}
	result += digitToChar(d,radix);
	i++;
    }
    return result;
}

SchemeObject* Parser::parseNumber(wstring s, uint32_t radix) {
    bool exact = false;
    if (s.size() == 0) {
	throw scheme_exception(L"Number formatting exception");
    }
    if (s[0] == L'#') {
	if (s.size() == 1) {
	    throw scheme_exception(L"Number formatting exception");
	}
	wchar_t prefix = s[1];
	switch(prefix) {
            case L'x' : radix = 16;
			break;
            case L'd' : radix = 10;
			break;
            case L'b' : radix = 2;
			break;
            case L'o' : radix = 8;
			break;
	    case L'e' :
	    case L'i' : throw scheme_exception(L"Exactness not supported");
	}
    }
}


