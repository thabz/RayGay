
#ifndef PARSER_MATERIAL_FACTORY_H
#define PARSER_MATERIAL_FACTORY_H

#include "scheme/scheme.h"

class MaterialFactory {
public:
  static SchemeObject *s_make_material(Scheme *scheme, SchemeObject *options);
  static void register_procs(Scheme *scheme);

private:
  static Scheme *scheme;
};

SchemeObject *s_material_p(Scheme *scheme, SchemeObject *object);

#endif
