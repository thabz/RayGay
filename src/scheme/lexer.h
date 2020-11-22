
#ifndef SCHEME_LEXER_H
#define SCHEME_LEXER_H

#include <iostream>
#include <list>
#include <stdint.h>
#include <string>

class SchemeObject;
class Scheme;

using namespace std;

class Lexer {
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
  Lexer(Scheme *);
  Token nextToken(SchemeObject *port);
  void putBack(Token token);
  Token peek(SchemeObject *port);
  wstring getString() { return str; };
  wstring getError() { return error; };
  SchemeObject *getNumber() { return number; };
  bool getBool() { return boolean; };
  wchar_t getChar() { return chr; };
  uint32_t getCurline() { return curline; };

private:
  bool isSymbolChar(wchar_t c);
  bool isDelimiter(wchar_t c);
  bool isWhitespace(wchar_t c);
  wchar_t readEscapedChar(SchemeObject *port);
  wchar_t readHexEscape(SchemeObject *port);
  wstring str;
  wstring error;
  SchemeObject *number;
  wchar_t chr;
  uint32_t curline;
  list<istream *> is_stack;
  list<int> curline_stack;
  list<Token> cache;
  bool boolean;
  Scheme *scheme;
};

#endif
