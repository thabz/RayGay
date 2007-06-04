#ifndef PARSER_CONVERTERS_H
#define PARSER_CONVERTERS_H

#include <vector>
#include <string>
#include <cassert>
#include "scheme/scheme.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "math/quaternion.h"
#include "image/rgb.h"
#include "image/rgba.h"

using namespace std;

double safe_scm2double(SchemeObject* o, int argnum, const char* procname);
int safe_scm2int(SchemeObject* o, int argnum, const char* procname);

Vector scm2vector(SchemeObject* s_vector, char* subr, int pos);

Vector2 scm2vector2(SchemeObject* s_vector, char* subr, int pos);

Quaternion scm2quaternion(SchemeObject* s_vector, char* subr, int pos);

SchemeObject* vector2scm(Vector vector);

RGB scm2rgb(SchemeObject* s_rgb);

RGBA scm2rgba(SchemeObject* s_rgb, char* subr, int pos);

SchemeObject* rgb2scm(RGB rgb);

vector<Vector> scm2vectorlist(SchemeObject* s_vector_vector, char* subr, int pos);

#endif
