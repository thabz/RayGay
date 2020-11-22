
#ifndef R6RS_LIB_LISTS_H
#define R6RS_LIB_LISTS_H

#include "scheme.h"

struct R6RSLibLists {
  static void bind(Scheme *scheme, SchemeObject *envt);
};

SchemeObject *s_memv(Scheme *scheme, SchemeObject *obj, SchemeObject *p);

#endif
