
#include "lexer.h"
#include <sstream>
#include <cctype>

Lexer::Lexer() {
    curline = 0;
}

int char_names_num = 2;
char* char_names[] = {"space","newline"};
char char_values[] = {' ', '\n'};

Lexer::Token Lexer::nextToken(istream* is) {
    char n,c;
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
        if (c == '\n') {
            curline++;
        }
        if (isspace(c)) {
            // Skip whitespace
            continue;
        }
        if (c == ';') {
            // Skip single-line comment until end of line or end of stream
            do {
                c = is->get();
            } while (c != '\n' && !is->eof());
            continue;
        }
        
        // Support for multiline nested comments as specified in SRFI 30.
        if (c == '#') {
            int d = is->get();
            if (d == '|') {
                // Skip (nested) multi-line comment
                int depth = 1;
                int p = 0;
                d = is->get();
                while (depth > 0) {
                    if (is->eof() || d == -1) {
                        cerr << "Unexpected end of input in nested comment" << endl;
                        return Lexer::ERROR;
                    }
                    if (p == '|' && d == '#') { 
                        depth--; p = 0; 
                    } else if (p == '#' && d == '|') { 
                        depth++; p = 0; 
                    }
                    p = d;
                    d = is->get();
                }
                continue;
            } else {
                is->unget();
            }
        }

        switch(c) {
            case '(':
                return Lexer::OPEN_PAREN;
            case ')':
                return Lexer::CLOSE_PAREN;
            case '\'':
                return Lexer::QUOTE;
            case '`':
                return Lexer::BACKQUOTE;
            case ',':
                n = is->get();
                if (n == '@') {
                    return Lexer::COMMA_AT;
                } else {
                    is->unget();
                    return Lexer::COMMA;
                }
            case '#': 
                n = is->get();
                if ((n == 'f' || n == 't')) {
					boolean = (n == 't');
					return Lexer::BOOLEAN;
                } else if (n == '(') {
					return Lexer::HASH_OPEN_PAREN;           
                } else if (n == '\\') {
                    for(int i = 0; i < char_names_num; i++) {
                        int j = 0;
                        char* p = char_names[i];
                        for(j = 0; *p != '\0'; p++, j++) {
                            if (*p != tolower(is->get())) {
                                break;
                            } else {
                                if (*(p+1) == '\0') {
                                    // Found a match
                                    // TODO: Check that next char in input is a closeparen or space
                                    chr = char_values[i];
                                    c = is->get();
                                    if (c == ' ' || c == ')' || is->eof()) {
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
                    //  Handle that if <character> in #\<character> is alphabetic, 
                    // then the character following <character> must be a delimiter 
                    // character such as a space or parenthesis.
                    chr = is->get();
                    c = is->get();
                    if (c == ' ' || c == ')' || is->eof()) {
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
            case '.' :
                c = is->get();
                if ((isspace(c))) {
                    return Lexer::PERIOD;
                } else {
                    is->unget();
                    break;
                }
            case '"':
                str = "";
                while (!is->eof()) {
                    c = is->get();
		            if (c == '\\') {
			            c = is->get();
                    } else if (c == '"') {
                        break;
		            }
		            str += c;
                }
                return Lexer::STRING;
        }
        if (isdigit(c)) {
            is->unget();
            (*is) >> number;
            return Lexer::NUMBER;
        }
        if (c == '-' || c == '+') {
            if (isdigit(is->peek())) {
                is->unget();
                (*is) >> number;
                return Lexer::NUMBER;
            }
        }
        
        // Read chars as a symbol
        str = c;
        while(!is->eof()) {
            char c = is->get();
	    if (c == -1 || is->eof()) {
		break;
	    }
	    if (!isSymbolChar(c)) {
                is->unget();
		break;
	    }
            str += c;
        }
        return Lexer::SYMBOL; 
    }
    // TODO: Check out why the stream has failed and throw exception
    return is->eof() ? Lexer::END : Lexer::ERROR;
}

bool Lexer::isSymbolChar(char c) {
    if (isspace(c) || c == ')' || c == '(' || c == ',' || c == '#') {
	return false;
    }
    return true;
}

void Lexer::putBack(Token token) {
    cache.push_front(token);
}
