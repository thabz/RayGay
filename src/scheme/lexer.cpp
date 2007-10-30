
#include "lexer.h"
#include <sstream>
#include <cctype>

Lexer::Lexer() {
    curline = 1;
}

int char_names_num = 2;
char* char_names[] = {"space","newline"};
char char_values[] = {' ', '\n'};

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
        cerr << "Lexer failed reading input-stream. File missing?" << endl;
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
        if (std::iswspace(c)) {
            // Skip whitespace
            continue;
        }
        if (c == L';') {
            // Skip single-line comment until end of line or end of stream
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
                        cerr << "Unexpected end of input in nested comment" << endl;
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
                } else if (n == L'(') {
		    return Lexer::HASH_OPEN_PAREN;           
                } else if (n == L'\\') {
                    for(int i = 0; i < char_names_num; i++) {
                        int j = 0;
                        char* p = char_names[i];
                        for(j = 0; *p != '\0'; p++, j++) {
                            if (*p != tolower(is->get())) {
                                break;
                            } else {
                                if (*(p+1) == L'\0') {
                                    // Found a match
                                    chr = char_values[i];
                                    c = is->get();
                                    if (c == L' ' || c == L')' || is->eof()) {
                                        is->unget();
                                        return Lexer::CHAR;
                                    } else {
                                        cerr << "Illegal char" << endl;
                                        return Lexer::ERROR;
                                    }
                                }
                            }
                        }
                        while (j-- >= 0) {
                            is->unget();
                        }
                    }
                    // Handle that if <character> in #\<character> is alphabetic, 
                    // then the character following <character> must be a delimiter 
                    // character such as a space or parenthesis.
                    chr = is->get();
                    c = is->get();
                    if (c == L' ' || c == L')' || is->eof()) {
                        is->unget();
                        return Lexer::CHAR;
                    } else {
                        cerr << "Illegal char" << endl;
                        return Lexer::ERROR;
                    }
                } else {
		    is->unget();
		    break;
                }
            case L'.' :
                c = is->get();
                if ((std::iswspace(c))) {
                    return Lexer::PERIOD;
                } else {
                    is->unget();
                    break;
                }
            case L'"':
                str = L"";
                while (!is->eof()) {
                    c = is->get();
		    if (c == L'\\') {
		        c = is->get();
                    } else if (c == L'"') {
                        break;
		    }
		    str += c;
                }
                return Lexer::STRING;
        }
        
        // Try reading as number
        std::wstreampos pos = is->tellg();
        is->unget();
	locale oldloc = is->getloc();
	is->imbue(locale("C"));
        (*is) >> number;
	is->imbue(oldloc);
        if (is->eof() || (!is->fail() && !isSymbolChar(is->peek()))) {
            return Lexer::NUMBER;        
        }
        is->clear();
        is->seekg(pos, ios::beg);
	if (is->fail()) {
	    cerr << "Seek failed. Fix number parser!" << endl;
	    return Lexer::ERROR;
	}
	

        // Read chars as a symbol
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
        return Lexer::SYMBOL; 
    }
    // TODO: Check out why the stream has failed and throw exception
    return is->eof() ? Lexer::END : Lexer::ERROR;
}

bool Lexer::isSymbolChar(wchar_t c) {
    if (std::iswspace(c) || c == L')' || c == L'(' || c == L',' || c == L'#' || c == L'\'' || c == L'`') {
	return false;
    }
    return true;
}

void Lexer::putBack(Token token) {
    cache.push_front(token);
}
