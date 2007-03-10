
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
    if (!cache.empty()) {
        Token t = cache.front();
        cache.pop_front();
        return t;
    }
    while(!is->fail()) 
    {
        char c = is->get();
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
            // Skip comment untill end of line or end of stream
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
                if (is->get() == '(')
                   return Lexer::QUOTE_OPEN_PAREN;
                else {
                    is->unget();
                    break;
                }
            case '"':
                str = "";
                while (!is->eof()) {
                    char c = is->get();
                    if (c == '"') {
                        break;
                    } else {
                        str += c;
                    }
                }
                return Lexer::STRING;
        }
        if (isdigit(c)) {
            is->unget();
            (*is) >> number;
            return Lexer::NUMBER;
        }
        
        // Read chars as a symbol
        str = c;
        while(!is->eof()) {
            char c = is->get();
            if (isspace(c)) {
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
    cout << "Error" << endl;
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
