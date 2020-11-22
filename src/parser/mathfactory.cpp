
#include <iostream>

#include "materials/perlin.h"
#include "math/halton.h"
#include "math/poisson_disc.h"
#include "parser/converters.h"
#include "parser/mathfactory.h"
#include "parser/wrapper.h"

SchemeObject *myrandom(Scheme *scheme, SchemeObject *s_min,
                       SchemeObject *s_max) {
  double min = safe_scm2double(s_min, 1, L"random");
  double max = safe_scm2double(s_max, 2, L"random");
  double result = RANDOM(min, max);
  return double2scm(result);
}

SchemeObject *noise(Scheme *scheme, SchemeObject *s_point) {
  Vector point = scm2vector(s_point, L"noise", 1);
  double n = Perlin::noise(point);
  return double2scm(n);
}

SchemeObject *noise3d(Scheme *scheme, SchemeObject *s_point,
                      SchemeObject *s_offset) {
  Vector point = scm2vector(s_point, L"noise3d", 1);
  double offset = safe_scm2double(s_offset, 2, L"noise3d");
  Vector v = Perlin::noise3d(point, offset);
  return vector2scm(v);
}

SchemeObject *vcross(Scheme *scheme, SchemeObject *s_v1, SchemeObject *s_v2) {
  Vector v1 = scm2vector(s_v1, L"vcross", 1);
  Vector v2 = scm2vector(s_v2, L"vcross", 2);
  Vector cross = Vector::xProduct(v1, v2);
  return vector2scm(cross);
}

SchemeObject *vdistance(Scheme *scheme, SchemeObject *s_v1,
                        SchemeObject *s_v2) {
  Vector v1 = scm2vector(s_v1, L"vdistance", 1);
  Vector v2 = scm2vector(s_v2, L"vdistance", 2);
  double d = (v1 - v2).length();
  return double2scm(d);
}

SchemeObject *vrandomunit(Scheme *scheme) {
  Vector v = Vector::randomUnitVector();
  return vector2scm(v);
}

SchemeObject *make_poisson_set(Scheme *scheme, SchemeObject *s_w,
                               SchemeObject *s_h, SchemeObject *s_r,
                               SchemeObject *s_num) {
  const wchar_t *proc = L"make-poisson-disc-set";
  double w = safe_scm2double(s_w, 1, proc);
  double h = safe_scm2double(s_h, 2, proc);
  double r = safe_scm2double(s_r, 3, proc);
  int num = safe_scm2int(s_num, 4, proc);
  Vector2 *set = new Vector2[num];
  PoissonDiscDistribution distr = PoissonDiscDistribution(w, h);
  int real_num = distr.createSet(r, num, set);
  SchemeObject *s_set = S_EMPTY_LIST;
  for (int i = 0; i < real_num; i++) {
    double x = set[i][0];
    double y = set[i][1];
    SchemeObject *s_point = i_list_2(double2scm(x), double2scm(y));
    s_set = i_cons(s_point, s_set);
  }
  return s_set;
}

SchemeObject *make_halton_set(Scheme *scheme, SchemeObject *s_w,
                              SchemeObject *s_h, SchemeObject *s_num) {
  const wchar_t *proc = L"make-halton-set";
  double w = safe_scm2double(s_w, 1, proc);
  double h = safe_scm2double(s_h, 2, proc);
  int num = safe_scm2int(s_num, 3, proc);
  Halton halton = Halton(2, 2);
  SchemeObject *s_set = S_EMPTY_LIST;
  for (int i = 0; i < num; i++) {
    double *values = halton.getNext();
    SchemeObject *s_point =
        i_list_2(double2scm(w * values[0]), double2scm(h * values[1]));
    s_set = i_cons(s_point, s_set);
  }
  return s_set;
}

void MathFactory::register_procs(Scheme *scheme) {
  scheme->assign(L"random", 2, 0, 0, (SchemeObject * (*)()) myrandom);
  scheme->assign(L"noise", 1, 0, 0, (SchemeObject * (*)()) noise);
  scheme->assign(L"noise3d", 2, 0, 0, (SchemeObject * (*)()) noise3d);
  scheme->assign(L"vcross", 2, 0, 0, (SchemeObject * (*)()) vcross);
  scheme->assign(L"vdistance", 2, 0, 0, (SchemeObject * (*)()) vdistance);
  scheme->assign(L"vrandomunit", 0, 0, 0, (SchemeObject * (*)()) vrandomunit);
  scheme->assign(L"make-poisson-disc-set", 4, 0, 0,
                 (SchemeObject * (*)()) make_poisson_set);
  scheme->assign(L"make-halton-set", 3, 0, 0,
                 (SchemeObject * (*)()) make_halton_set);
}
