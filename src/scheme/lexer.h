
#ifndef SCHEME_LEXER_H
#define SCHEME_LEXER_H

#include <string>
#include <list>
#include <iostream>

class SchemeObject;

using namespace std;

class Lexer 
{
    public:
        enum Token {
            OPEN_PAREN,
            CLOSE_PAREN,
            OPEN_BRACKET,
            CLOSE_BRACKET,
            HASH_OPEN_PAREN,
            VU8_OPEN_PAREN,
            SYMBOL,
            NUMBER,
            STRING,
            CHAR,
            BOOLEAN,
            QUOTE,
            BACKQUOTE,
            COMMA,
            COMMA_AT,
            PERIOD,
	        DATUM_COMMENT,
            ERROR,
            END
        };
        Lexer();
        Token nextToken(wistream* is);
        void putBack(Token token);
        Token peek(wistream* is);
        wstring getString() { return str; };
        wstring getError() { return error; };
        SchemeObject* getNumber() { return number; };
        bool getBool() { return boolean; };
        wchar_t getChar() { return chr; };
        uint32_t getCurline() { return curline; };
        
    private:
        bool isSymbolChar(wchar_t c);
	    bool isDelimiter(wchar_t c);
	    bool isWhitespace(wchar_t c);
        wchar_t readEscapedChar(wistream* is);
        wchar_t readHexEscape(wistream* is);
        wstring str;
        wstring error;
        SchemeObject* number;
        wchar_t chr;
        uint32_t curline;
        list<istream*> is_stack;
        list<int> curline_stack;
        list<Token> cache;
        bool boolean;
};

#endif

