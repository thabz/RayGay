#ifndef PARSER_CONVERTERS_H
#define PARSER_CONVERTERS_H

#include <vector>
#include <string>
#include <cassert>
#include <libguile.h>
#include <guile/gh.h>
#include "math/vector.h"
#include "math/vector2.h"
#include "image/rgb.h"

using namespace std;

Vector scm2vector(SCM s_vector, char* subr, int pos);

Vector2 scm2vector2(SCM s_vector, char* subr, int pos);

SCM vector2scm(Vector vector);

RGB scm2rgb(SCM s_rgb);

SCM rgb2scm(RGB rgb);

vector<Vector> scm2vectorlist(SCM s_vector_vector, char* subr, int pos);

string scm2string(SCM s_string);

#endif
