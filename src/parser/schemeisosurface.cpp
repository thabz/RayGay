
#include "parser/schemeisosurface.h"
#include "profiler.h"

pthread_mutex_t SchemeIsosurface::mutex;
bool SchemeIsosurface::mutex_initialized = false;
Profiler *SchemeIsosurface::profiler = NULL;

SchemeIsosurface::SchemeIsosurface(Scheme *scheme, SchemeObject *procedure,
                                   AABox bbox, uint32_t steps, double accuracy,
                                   double iso, Material *mat)
    : IsoSurface(steps, accuracy, iso, mat) {
  this->bbox = bbox;
  this->procedure = procedure;
  this->scheme = scheme;

  if (this->profiler == NULL) {
    this->profiler = Profiler::create("Scheme iso-callbacks", "Rendering");
  }

  if (!mutex_initialized) {
    pthread_mutex_init(&mutex, NULL);
    mutex_initialized = true;
  }
}

AABox SchemeIsosurface::_getBoundingBox() const { return bbox; }

double SchemeIsosurface::evaluateFunction(const Vector &point) const {
  pthread_mutex_lock(&mutex);

  profiler->start();
  SchemeObject *x = double2scm(point[0]);
  SchemeObject *y = double2scm(point[1]);
  SchemeObject *z = double2scm(point[2]);
  SchemeObject *s_result = scheme->callProcedure_3(procedure, x, y, z);
  if (i_number_p(s_result) == S_FALSE) {
    throw scheme_exception(L"iso-function didn't return a number: " +
                           procedure->toString());
  }
  double result = scm2double(s_result);
  profiler->stop();
  pthread_mutex_unlock(&mutex);
  return result;
}

SceneObject *SchemeIsosurface::clone() const {
  SchemeIsosurface *result = new SchemeIsosurface(*this);
  return result;
}
