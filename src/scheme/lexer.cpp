
#include "lexer.h"
#include "scheme.h"
#include "numbers.h"
#include <sstream>
#include <cctype>

Lexer::Lexer() {
    curline = 1;
}

Lexer::Token Lexer::nextToken(wistream* is) {
    wchar_t n,c;
    if (!cache.empty()) {
        Token t = cache.front();
        cache.pop_front();
        return t;
    }
    if (is->eof()) {
        return Lexer::END;
    }
    if (is->fail()) {
        error = L"Lexer failed reading input-stream. File missing?";
        return Lexer::ERROR;
    }
    while(!is->fail()) 
    {
        c = is->get();
        if (is->eof() || c == -1) {
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
        if (c == L';' || (c == L'#') && is->peek() == L'!') {
            do {
                c = is->get();
            } while (c != L'\n' && !is->eof());
            if (c == L'\n') {
                curline++;
            }
            continue;
        }
        
        // Support for multiline nested comments as specified in SRFI 30.
	    // Support for datum comments
        if (c == L'#') {
            int d = is->get();
            if (d == L'|') {
                // Skip (nested) multi-line comment
                int depth = 1;
                int p = 0;
                d = is->get();
                if (d == L'\n') {
                    curline++;    
                }
                while (depth > 0) {
                    if (is->eof() || d == -1) {
                        error = L"Unexpected end of input in nested comment";
                        return Lexer::ERROR;
                    }
                    if (p == L'|' && d == L'#') { 
                        depth--; p = 0; 
                    } else if (p == L'#' && d == L'|') { 
                        depth++; p = 0; 
                    }
                    p = d;
                    d = is->get();
                    if (d == L'\n') {
                        curline++;    
                    }
                }
                continue;
	        } else if (d == L';') {
	            return Lexer::DATUM_COMMENT;	
            } else {
                is->unget();
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
                n = is->get();
                if (n == L'@') {
                    return Lexer::COMMA_AT;
                } else {
                    is->unget();
                    return Lexer::COMMA;
                }
            case L'#': 
                n = is->get();
                if ((n == L'f' || n == L't')) {
		            boolean = (n == L't');
		            return Lexer::BOOLEAN;
                } else if (n == L'v') {
                    wstring collected;
                    collected += is->get();
                    while (is->peek() != L'(' && !is->eof()) {
                        collected += is->get();
                    }
                    if (collected == L"u8" && is->get() == L'(') {
                        return Lexer::VU8_OPEN_PAREN;
                    } else {
                        error = L"Illegal bytevector literal";
                        return Lexer::ERROR;
                    }
                } else if (n == L'(') {
		            return Lexer::HASH_OPEN_PAREN;           
                } else if (n == L'\\') {
                    wstring collected;
                    collected += is->get();
                    while (!(isDelimiter(is->peek()) || is->eof())) {
                        collected += is->get();
                    }
                    if (collected.size() == 0) {
                        error = L"Illegal char";
                        return Lexer::ERROR;
                    } else if (collected.size() == 1) {
                        chr = collected[0];
                        return Lexer::CHAR;
                    } else {
                        if (collected[0] == L'x' || collected[0] == L'X') {
                            is->clear();
                            for(uint32_t i = 0; i < collected.size()-1; i++) {
                                is->unget();
                            }
                            chr = readHexEscape(is);
                            wchar_t tmp = is->get();
                            if (chr >= 0 && (tmp == -1 || isDelimiter(tmp) || is->eof() || is->fail())) {
                                is->unget();
                                return Lexer::CHAR;
                            } else {
                                is->unget();
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
		            is->unget();
		            break;
                }
            case L'.' : 
                if (isWhitespace(is->peek())) {
                    is->ignore();
                    return Lexer::PERIOD;
                }
                break;
            case L'"':
                str = L"";
                while (!is->eof()) {
                    c = is->get();
		            if (c == L'\\') {
                        c = readEscapedChar(is);
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
        while(!is->eof()) {
            wchar_t e;
	        e = is->get();
	        if (e == -1 || is->eof()) {
	    	    break;
	        }
	        if (!isSymbolChar(e)) {
                is->unget();
	    	    break;
	        }
	        ss << e;
        }
	    str = ss.str();

	    number = i_string_2_number(str,10);
	    if (number == S_FALSE) {
	        return Lexer::SYMBOL; 
	    } else {
	        return Lexer::NUMBER;
	    }
    }
    if (is->eof()) {
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

Lexer::Token Lexer::peek(wistream* is) {
    Token token = nextToken(is);
    putBack(token);
    return token;
}

const wstring escape_sequences(L"abtnvfr\"\\");
const wchar_t escape_values[] = {0x7,0x8,0x9,0xA,0xB,0xC,0xD,0x22,0x5C}; 

wchar_t Lexer::readEscapedChar(wistream* is) {
    wchar_t c = is->get();
    if (is->eof()) {
        error = L"Unexpected end of file";
        return -1;
    }
    wstring::size_type pos = escape_sequences.find(c);
    if (pos != escape_sequences.npos) {
        return escape_values[pos];
    } else if (c == L'x') {
        wchar_t result = readHexEscape(is);
        if (!is->eof() && is->get() == L';') {
            return result;
        }
    }
    error = L"Unvalid char-escape";
    return -1;
}

wchar_t Lexer::readHexEscape(wistream* is) {
    int64_t n;
    (*is) >> hex >> n;
    // Check that n is in legal unicode range
    if (n < 0 || (n >= 0xD800 && n < 0xE000) || n >= 0x110000) {
        error = L"Escaped char in excluded unicode range";
        return -1;
    }
    return wchar_t(n);
}
