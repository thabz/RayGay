
#ifndef SCHEME_SYNTAX_H
#define SCHEME_SYNTAX_H

// These are bound to names by define-syntax, let-syntax or letrec-syntax
class Transformer {};

class Syntax {
public:
  static SchemeObject *transform(SchemeObject *inputprogram);

private:
  static bool matches(SchemeObject *literals, SchemeObject *pattern,
                      SchemeObject *input);
  static SchemeObject *transform(SchemeObject *pattern, SchemeObject *templat,
                                 SchemeObject *input);
};

#endif
