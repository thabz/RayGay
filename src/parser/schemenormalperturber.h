
#ifndef PARSER_SCHEME_NORMAL_PERTURBER_H
#define PARSER_SCHEME_NORMAL_PERTURBER_H

#include "materials/normalperturbers/normalperturber.h"
#include "scheme/scheme.h"

class Profiler;

class SchemeNormalPerturber : public NormalPerturber {

public:
  SchemeNormalPerturber(Scheme *scheme, SchemeObject *procedure);
  ~SchemeNormalPerturber();

protected:
  Vector _perturb(const Vector &point, const Vector &normal) const;

private:
  Scheme *scheme;
  SchemeObject *s_procedure;
  static Profiler *profiler;
  static pthread_mutex_t mutex;
  static bool mutex_initialized;
};

#endif
