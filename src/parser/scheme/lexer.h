
#ifndef SCHEME_LEXER_H
#define SCHEME_LEXER_H

#include <string>
#include <istream>

using namespace std;



class Lexer 
{
    public:
        enum {
            OPEN_PAREN,
            CLOSE_PAREN,
            SYMBOL,
            NUMBER,
            STRING,
            BOOLEAN,
            COMMA_OPEN_PAREN,
            QUOTE_OPEN_PAREN,
            BACKQUOTE_OPEN_PAREN
        } Token;

        Lexer(string data);
        Token nextToken();
        string getString();
        double getNumber();
        bool getBool();
        
    private:
        istream is;
        string str;
        double number;
        bool boolean;
};

#endif