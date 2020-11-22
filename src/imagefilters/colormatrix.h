
#ifndef FILTERS_COLOR_MATRIX_H
#define FILTERS_COLOR_MATRIX_H

#include "image/rgba.h"

class ColorMatrix {

public:
  ColorMatrix();
  ColorMatrix(double c[25]);
  RGBA operator*(const RGBA &col) const;

private:
  double matrix[25];
};

#endif
