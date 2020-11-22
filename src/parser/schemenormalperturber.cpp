
#include "parser/schemenormalperturber.h"
#include "parser/converters.h"
#include "profiler.h"

pthread_mutex_t SchemeNormalPerturber::mutex;
bool SchemeNormalPerturber::mutex_initialized = false;
Profiler *SchemeNormalPerturber::profiler = NULL;

SchemeNormalPerturber::SchemeNormalPerturber(Scheme *scheme,
                                             SchemeObject *procedure) {
  this->s_procedure = procedure;
  this->scheme = scheme;
  if (this->profiler == NULL) {
    this->profiler = Profiler::create("Scheme perturb-callbacks", "Rendering");
  }
  if (!mutex_initialized) {
    pthread_mutex_init(&mutex, NULL);
    mutex_initialized = true;
  }
}

SchemeNormalPerturber::~SchemeNormalPerturber() {
  if (mutex_initialized) {
    pthread_mutex_destroy(&mutex);
    mutex_initialized = false;
  }
}

Vector SchemeNormalPerturber::_perturb(const Vector &P, const Vector &N) const {
  pthread_mutex_lock(&mutex);
  profiler->start();
  SchemeObject *s_point = vector2scm(P);
  SchemeObject *s_normal = vector2scm(N);
  SchemeObject *s_result =
      scheme->callProcedure_2(s_procedure, s_point, s_normal);
  Vector result = scm2vector(s_result, NULL, 0);
  profiler->stop();
  pthread_mutex_unlock(&mutex);
  result.normalize();
  return result;
}
