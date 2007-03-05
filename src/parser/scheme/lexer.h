
#ifndef SCHEME_LEXER_H
#define SCHEME_LEXER_H

#include <string>
#include <istream>
#include <iostream>

using namespace std;

class Lexer 
{
    public:
        enum Token {
            OPEN_PAREN,
            CLOSE_PAREN,
            SYMBOL,
            NUMBER,
            STRING,
            BOOLEAN,
            COMMA_OPEN_PAREN,
            HASH_OPEN_PAREN,
            QUOTE,
            BACKQUOTE_OPEN_PAREN,
            ERROR,
            END
        };

        Lexer(string data);
        Lexer(istream* is);
        Token nextToken();
        string getString() { return str; };
        double getNumber() { return number; };
        bool getBool() { return boolean; };
        
    private:
        istream* is;
        string str;
        double number;
        bool boolean;
};

#endif