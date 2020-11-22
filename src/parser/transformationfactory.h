
#ifndef PARSER_TRANSFORMATION_FACTORY_H
#define PARSER_TRANSFORMATION_FACTORY_H

#include "scheme/scheme.h"

class Matrix;

class TransformationFactory {

public:
  static SchemeObject *rotate(Scheme *scheme, SchemeObject *obj,
                              SchemeObject *axis, SchemeObject *angle);
  static SchemeObject *translate(Scheme *scheme, SchemeObject *obj,
                                 SchemeObject *translation);
  static SchemeObject *scale(Scheme *scheme, SchemeObject *obj,
                             SchemeObject *scale);
  static SchemeObject *orient(Scheme *scheme, SchemeObject *s_obj,
                              SchemeObject *s_direction_x,
                              SchemeObject *s_direction_y,
                              SchemeObject *s_direction_z);

  static void register_procs(Scheme *scheme);

private:
  static SchemeObject *transform(Scheme *scheme, SchemeObject *obj,
                                 const Matrix &m, const wchar_t *subr);
};

#endif
