
#include "lexer.h"
#include <sstream>
#include <cctype>

Lexer::Lexer(string data) {
    this->is = new istringstream(data);
};


Lexer::Lexer(istream* is) {
    this->is = is;
}

Lexer::Token Lexer::nextToken() {
    // Skip whitespace
    while(!is->eof() && !is->fail()) {
        char c = is->get();
        if (isspace(c)) {
            continue;
        }
        if (c == -1) {
            return Lexer::END;
        }
        switch(c) {
            case '(':
                return Lexer::OPEN_PAREN;
            case ')':
                return Lexer::CLOSE_PAREN;
            case '\'':
                return Lexer::QUOTE;
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
    if (is->eof()) {
        return Lexer::END;
    }
    cout << "Error" << endl;
    return Lexer::ERROR;
}