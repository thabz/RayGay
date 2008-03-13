
#include <sstream>
#include <cctype>

#include "lexer.h"
#include "scheme.h"
#include "numbers.h"
#include "r6rs-lib-io-ports.h"

Lexer::Lexer(Scheme* scheme) {
    curline = 1;
    this->scheme = scheme;
}

Lexer::Token Lexer::nextToken(SchemeObject* port) {
    wchar_t n,c;
    if (!cache.empty()) {
        Token t = cache.front();
        cache.pop_front();
        return t;
    }
    if (i_port_eof_p(port) == S_TRUE) {
        return Lexer::END;
    }

    while(true) 
    {
        c = i_get_char(port);
        if (c == -1) {
            return Lexer::END;
        }
        if (c == L'\n') {
            curline++;
	        continue;
        }
        if (isWhitespace(c)) {
            // Skip whitespace
            continue;
        }
        // Skip single-line comment until end of line or end of stream
        // Or skip #! lines
        if (c == L';' || (c == L'#') && i_lookahead_char(port) == L'!') {
            do {
                c = i_get_char(port);
            } while (c != L'\n' && i_port_eof_p(port) == S_FALSE);
            if (c == L'\n') {
                curline++;
            }
            continue;
        }
        
        // Support for multiline nested comments as specified in SRFI 30.
	    // Support for datum comments
        if (c == L'#') {
            int d = i_get_char(port);
            if (d == L'|') {
                // Skip (nested) multi-line comment
                int depth = 1;
                int p = 0;
                d = i_get_char(port);
                if (d == L'\n') {
                    curline++;    
                }
                while (depth > 0) {
                    if (d == -1) {
                        error = L"Unexpected end of input in nested comment";
                        return Lexer::ERROR;
                    }
                    if (p == L'|' && d == L'#') { 
                        depth--; p = 0; 
                    } else if (p == L'#' && d == L'|') { 
                        depth++; p = 0; 
                    }
                    p = d;
                    d = i_get_char(port);
                    if (d == L'\n') {
                        curline++;    
                    }
                }
                continue;
	        } else if (d == L';') {
	            return Lexer::DATUM_COMMENT;	
            } else {
                i_unget_char(port);
            }
        }

        switch(c) {
            case L'(':
                return Lexer::OPEN_PAREN;
            case L')':
                return Lexer::CLOSE_PAREN;
            case L'[':
                return Lexer::OPEN_BRACKET;
            case L']':
                return Lexer::CLOSE_BRACKET;
            case L'\'':
                return Lexer::QUOTE;
            case L'`':
                return Lexer::BACKQUOTE;
            case L',':
                n = i_get_char(port);
                if (n == L'@') {
                    return Lexer::COMMA_AT;
                } else {
                    i_unget_char(port);
                    return Lexer::COMMA;
                }
            case L'#': 
                n = i_get_char(port);
                if ((n == L'f' || n == L't')) {
		            boolean = (n == L't');
		            return Lexer::BOOLEAN;
                } else if (n == L'v') {
                    wstring collected;
                    collected += i_get_char(port);
                    while (i_lookahead_char(port) != L'(' && i_port_eof_p(port) == S_FALSE) {
                        collected += i_get_char(port);
                    }
                    if (collected == L"u8" && i_get_char(port) == L'(') {
                        return Lexer::VU8_OPEN_PAREN;
                    } else {
                        error = L"Illegal bytevector literal";
                        return Lexer::ERROR;
                    }
                } else if (n == L'(') {
		            return Lexer::HASH_OPEN_PAREN;           
                } else if (n == L'\\') {
                    wstring collected;
                    collected += i_get_char(port);
                    while (!(isDelimiter(i_lookahead_char(port)) || i_lookahead_char(port) == -1)) {
                        collected += i_get_char(port);
                    }
                    if (collected.size() == 0) {
                        error = L"Illegal char";
                        return Lexer::ERROR;
                    } else if (collected.size() == 1) {
                        chr = collected[0];
                        return Lexer::CHAR;
                    } else {
                        if (collected[0] == L'x' || collected[0] == L'X') {
                            for(uint32_t i = 0; i < collected.size()-1; i++) {
                                i_unget_char(port);
                            }
                            chr = readHexEscape(port);
                            wchar_t tmp = i_lookahead_char(port);
                            if (chr >= 0 && (tmp == -1 || isDelimiter(tmp))) {
                                return Lexer::CHAR;
                            } else {
                                error = L"Illegal char: \\" + collected;
                                return Lexer::ERROR;
                            }
                        } else {
                            chr = SchemeObject::charname2char(collected);
                            if (chr >= 0) {
                                return Lexer::CHAR;
                            } else {
                                error = L"Illegal charname: " + collected;
                                return Lexer::ERROR;
                            }
                        }
                    }
                } else {
		            i_unget_char(port);
		            break;
                }
            case L'.' : 
                if (isWhitespace(i_lookahead_char(port))) {
                    i_get_char(port);
                    return Lexer::PERIOD;
                }
                break;
            case L'"':
                str = L"";
                while (i_port_eof_p(port) == S_FALSE) {
                    c = i_get_char(port);
		            if (c == L'\\') {
                        c = readEscapedChar(port);
                        if (c == -1) {
                            return Lexer::ERROR;
                        }
                    } else if (c == L'"') {
                        break;
		            }
		            str += c;
                }
                return Lexer::STRING;
        }

	    // Extract a wstring s until reaching a
	    // non-isSymbolChar() or is->eof() or is->fail()
	    // Return a number if i_string_2_number(s,10) succeeds
	    // otherwise return the wstring as a symbol
        
	    wstringstream ss;
	    ss << c;
        while(i_port_eof_p(port) == S_FALSE) {
            wchar_t e;
	        e = i_get_char(port);
	        if (e == -1) {
	    	    break;
	        }
	        if (!isSymbolChar(e)) {
                i_unget_char(port);
	    	    break;
	        }
	        ss << e;
        }
	    str = ss.str();

	    number = i_string_2_number(scheme, str,10);
	    if (number == S_FALSE) {
	        return Lexer::SYMBOL; 
	    } else {
	        return Lexer::NUMBER;
	    }
    }
    if (i_port_eof_p(port) == S_TRUE) {
        return Lexer::END;
    } else {
        error = L"Unknown error reading inputstream";
        return Lexer::ERROR;
    }
}

bool Lexer::isSymbolChar(wchar_t c) {
    return !(isWhitespace(c) || c == L')' || c == L'(' || c == L']' || 
            c == L'[' || c == L',' || c == L'#' || c == L'\'' || c == L'`');
}

bool Lexer::isDelimiter(wchar_t c) {
    return c == L'(' || c == L')' || 
           c == L'[' || c == L']' || 
           c == L'"' || c == L';' || 
           c == L'#' || isWhitespace(c);
}

bool Lexer::isWhitespace(wchar_t c) {
    return std::iswspace(c);
    // or any character whose category is Zs, Zl, or Zp.
}

void Lexer::putBack(Token token) {
    cache.push_front(token);
}

Lexer::Token Lexer::peek(SchemeObject* port) {
    Token token = nextToken(port);
    putBack(token);
    return token;
}

const wstring escape_sequences(L"abtnvfr\"\\");
const wchar_t escape_values[] = {0x7,0x8,0x9,0xA,0xB,0xC,0xD,0x22,0x5C}; 

wchar_t Lexer::readEscapedChar(SchemeObject* port) {
    wchar_t c = i_get_char(port);
    if (i_port_eof_p(port) == S_TRUE) {
        error = L"Unexpected end of file";
        return -1;
    }
    wstring::size_type pos = escape_sequences.find(c);
    if (pos != escape_sequences.npos) {
        return escape_values[pos];
    } else if (c == L'x') {
        wchar_t result = readHexEscape(port);
        if (i_port_eof_p(port) == S_FALSE && i_get_char(port) == L';') {
            return result;
        }
    }
    error = L"Unvalid char-escape";
    return -1;
}

wchar_t Lexer::readHexEscape(SchemeObject* port) {
    uint64_t n = 0;
    int digits_found = 0;
    wchar_t c;
    while(true) {
	    c = i_get_char(port);
	    if (c == -1) {
		    break;
	    } else if (c >= L'0' && c <= L'9') {
		    n = (n << 4) | (c - L'0');
		    digits_found++;
	    } else if (c >= L'a' && c <= L'f') {
		    n = (n << 4) | (c + 10 - L'a');
		    digits_found++;
	    } else if (c >= L'A' && c <= L'F') {
		    n = (n << 4) | (c + 10 - L'A');
		    digits_found++;
	    } else {
		    i_unget_char(port);
		    break;
	    }
    }
    if (digits_found == 0) {
	    return -1;
    }

    // Check that n is in legal unicode range
    if ((n >= 0xD800 && n < 0xE000) || n >= 0x110000) {
        error = L"Escaped char in excluded unicode range";
        return -1;
    }
    return wchar_t(n);
}

