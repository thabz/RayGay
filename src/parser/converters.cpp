
#include "parser/converters.h"
#include "scheme/numbers.h"

Vector scm2vector(SchemeObject *s_vector, const wchar_t *subr, int pos) {
  if (!(scm2bool(i_vector_p(s_vector)) && i_vector_length(s_vector) == 3)) {
    wrong_type_arg(subr, pos, s_vector);
  }

  Vector result;
  SchemeObject *s_value;
  for (uint32_t i = 0; i < 3; i++) {
    s_value = i_vector_ref(s_vector, i);
    result[i] = safe_scm2double(s_value, pos, subr);
  }
  return result;
}

Quaternion scm2quaternion(SchemeObject *s_vector, const wchar_t *subr,
                          int pos) {
  if (!(scm2bool(i_vector_p(s_vector)) && i_vector_length(s_vector) == 4)) {
    wrong_type_arg(subr, pos, s_vector);
  }

  double r[4];
  SchemeObject *s_value;
  for (uint32_t i = 0; i < 4; i++) {
    s_value = i_vector_ref(s_vector, i);
    r[i] = safe_scm2double(s_value, pos, subr);
  }
  return Quaternion(r[0], r[1], r[2], r[3]);
}

RGBA scm2rgba(SchemeObject *s_vector, const wchar_t *subr, int pos) {
  if (!((scm2bool(i_vector_p(s_vector))))) {
    wrong_type_arg(subr, pos, s_vector);
  };
  int l = i_vector_length(s_vector);
  if (l == 3) {
    return RGBA(scm2rgb(s_vector));
  }
  if (l != 4) {
    wrong_type_arg(subr, pos, s_vector);
  }

  double r[4];
  SchemeObject *s_value;
  for (uint32_t i = 0; i < 4; i++) {
    s_value = i_vector_ref(s_vector, i);
    r[i] = safe_scm2double(s_value, pos, subr);
  }
  return RGBA(r[0], r[1], r[2], r[3]);
}

Vector2 scm2vector2(SchemeObject *s_vector, const wchar_t *subr, int pos) {
  if (!(scm2bool(i_vector_p(s_vector)) && i_vector_length(s_vector) == 2)) {
    wrong_type_arg(subr, pos, s_vector);
  }

  double res[2];
  SchemeObject *s_value;
  for (uint32_t i = 0; i < 2; i++) {
    s_value = i_vector_ref(s_vector, i);
    res[i] = safe_scm2double(s_value, pos, subr);
  }
  return Vector2(res[0], res[1]);
}

SchemeObject *vector2scm(Vector vector) {
  SchemeObject *v0 = double2scm(vector[0]);
  SchemeObject *v1 = double2scm(vector[1]);
  SchemeObject *v2 = double2scm(vector[2]);
  SchemeObject *result = i_make_vector(3, S_UNSPECIFIED);
  i_vector_set_e(result, 0, v0);
  i_vector_set_e(result, 1, v1);
  i_vector_set_e(result, 2, v2);
  return result;
}

RGB scm2rgb(SchemeObject *s_rgb) {
  Vector v = scm2vector(s_rgb, L"unknown", 0);
  return RGB(v[0], v[1], v[2]);
}

SchemeObject *rgb2scm(RGB rgb) {
  SchemeObject *r = double2scm(rgb.r());
  SchemeObject *g = double2scm(rgb.g());
  SchemeObject *b = double2scm(rgb.b());
  SchemeObject *result = i_make_vector(3, S_UNSPECIFIED);
  i_vector_set_e(result, 0, r);
  i_vector_set_e(result, 1, g);
  i_vector_set_e(result, 2, b);
  return result;
}

vector<Vector> scm2vectorlist(SchemeObject *s_list_vector, const wchar_t *subr,
                              int pos) {
  assert(scm2bool(i_list_p(s_list_vector)));
  uint32_t length = i_length(s_list_vector);
  SchemeObject *s_vector;
  vector<Vector> result;
  for (uint32_t i = 0; i < length; i++) {
    s_vector = i_list_ref(s_list_vector, int2scm(i));
    result.push_back(scm2vector(s_vector, subr, pos));
  }
  return result;
}

double safe_scm2double(SchemeObject *o, int argnum, const wchar_t *procname) {
  assert_arg_number_type(procname, argnum, o);
  return scm2double(o);
}

int safe_scm2int(SchemeObject *o, int argnum, const wchar_t *procname) {
  assert_arg_int_type(procname, argnum, o);
  return scm2int(o);
}

uint32_t safe_scm2uint(SchemeObject *o, int argnum, const wchar_t *procname) {
  assert_arg_non_negative_int(procname, argnum, o);
  return scm2int(o);
}

/**
 * Takes a list of options (such as ('diffuse #(0 0 0) 'kd 0 'ks 0)) and
 * returns them as a hash.
vector<std::wstring,SchemeObject*> args2hash(Scheme* scheme, const wchar_t*
proc, SchemeObject* s_options) { vector<std::wstring,SchemeObject*> h;

    if (!scm2bool(s_list_p (scheme, s_options))) {
            wrong_type_arg (proc, 1, s_options);
    }
    uint32_t length = safe_scm2int(s_length(scheme, s_options), 0, L"");

    assert(length % 2 == 0);
    uint32_t argc = length / 2;

    for(uint32_t i = 0; i < argc; i++) {
        SchemeObject* s_key = s_list_ref(scheme, s_options, int2scm(i*2));
        if (i_symbol_p(s_key) == S_FALSE) {
            throw scheme_exception(L"Invalid option-name in : " +
(wstring(proc)) + s_key->toString());
        }
        wstring key = s_key->toString();
        SchemeObject* s_value = s_list_ref(scheme, s_options, int2scm(i*2+1));
        h[key] = s_value;
    }
    return h;
}
 */
