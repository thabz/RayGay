
#include "lexer.h"
#include <sstream>
#include <cctype>

// Used for testing
Lexer::Lexer(string data) {
    this->is = new istringstream(data);
    curline = 0;
};


Lexer::Lexer(istream* is) {
    this->is = is;
    curline = 0;
}

Lexer::Token Lexer::nextToken() {
    char n,c;
    if (!cache.empty()) {
        Token t = cache.front();
        cache.pop_front();
        return t;
    }
    if (is->eof()) {
        if (!popInputStream()) {
            return Lexer::END;
        }
    }
    while(!is->fail()) 
    {
        c = is->get();
        if (is->eof() || c == -1) {
            if (popInputStream()) {
                continue;
            } else {
                return Lexer::END;
            }
        }
        if (c == '\n') {
            curline++;
        }
        if (isspace(c)) {
            // Skip whitespace
            continue;
        }
        if (c == ';') {
            // Skip comment until end of line or end of stream
            do {
                c = is->get();
            } while (c != '\n' && !is->eof());
            continue;
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
            if (isspace(c) || c == -1 || is->eof()) {
                break;
            }
            if (c == ')') {
                is->unget();
                break;
            }
            str += c;
        }
        return Lexer::SYMBOL; 
    }
    // TODO: Check out why the stream has failed and throw exception
    return Lexer::ERROR;
}

void Lexer::putBack(Token token) {
    cache.push_front(token);
}

void Lexer::pushInputStream(istream* new_is) {
    is_stack.push_front(is);
    curline_stack.push_front(curline);
    is = new_is;
    curline = 0;
}

bool Lexer::popInputStream() {
    if (!is_stack.empty()) {
        curline = curline_stack.front();
        curline_stack.pop_front();
        is = is_stack.front();
        is_stack.pop_front();
        return true;
    }
    return false;
}
