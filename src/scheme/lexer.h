
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
            CHAR,
            BOOLEAN,
            HASH_OPEN_PAREN,
            QUOTE,
            BACKQUOTE,
            COMMA,
            COMMA_AT,
            PERIOD,
            ERROR,
            END
        };
        Lexer::Lexer();
        Token nextToken(istream* is);
        void putBack(Token token);
        string getString() { return str; };
        double getNumber() { return number; };
        bool getBool() { return boolean; };
        char getChar() { return chr; };
        int getCurline() { return curline; };
        
    private:
	bool isSymbolChar(char c);
        string str;
        double number;
        bool boolean;
        char chr;
        int curline;
        list<istream*> is_stack;
        list<int> curline_stack;
        list<Token> cache;
};

#endif

