
#ifndef SCHEME_LEXER_H
#define SCHEME_LEXER_H

#include <string>
#include <list>
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
            HASH_OPEN_PAREN,
            COMMA_OPEN_PAREN,
            QUOTE,
            BACKQUOTE_OPEN_PAREN,
            PERIOD,
            ERROR,
            END
        };

        Lexer(string data);
        Lexer(istream* is);
        Token nextToken();
        void putBack(Token token);
        string getString() { return str; };
        double getNumber() { return number; };
        bool getBool() { return boolean; };
        int getCurline() { return curline; };
        
        void pushInputStream(istream* is);
        
    private:
        bool popInputStream();
        
        istream* is;
        string str;
        double number;
        bool boolean;
        int curline;
        list<istream*> is_stack;
        list<int> curline_stack;
        list<Token> cache;
        
};

#endif

