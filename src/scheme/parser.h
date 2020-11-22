
#ifndef SCHEME_PARSER_H
#define SCHEME_PARSER_H

#include "lexer.h"
#include "objects.h"
#include "scheme.h"

class Parser {
public:
  Parser(Scheme *scheme);
  SchemeObject *parse(SchemeObject *port);
  SchemeObject *read(SchemeObject *port);

private:
  SchemeObject *read_list(SchemeObject *port);
  SchemeObject *read_quoted(SchemeObject *port);
  SchemeObject *read_unquoted(SchemeObject *port);
  SchemeObject *read_unquote_spliced(SchemeObject *port);
  SchemeObject *read_quasiquoted(SchemeObject *port);

  void decorateWithLineNumber(SchemeObject *obj, uint32_t linenumber);

  Lexer *lexer;
  Scheme *scheme;
};

#endif
