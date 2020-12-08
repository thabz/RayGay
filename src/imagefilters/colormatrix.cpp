
#include "imagefilters/colormatrix.h"
#include "image/rgba.h"

ColorMatrix::ColorMatrix() {
  for (uint32_t i = 0; i < 25; i++) {
    matrix[i] = 0.0;
  }
  for (uint32_t i = 0; i < 5; i++) {
    matrix[i * 5 + i] = 1.0;
  }
}

ColorMatrix::ColorMatrix(double c[25]) {
  for (uint32_t i = 0; i < 25; i++) {
    matrix[i] = c[i];
  }
}

RGBA ColorMatrix::operator*(const RGBA &color) const {
  double result[4] = {0, 0, 0, 0};
  double c[4] = {color.r(), color.g(), color.b(), color.a()};

  for (int y = 0; y < 4; y++) {
    result[y] = matrix[y * 4 + 0] * c[0] + matrix[y * 4 + 1] * c[1] +
                matrix[y * 4 + 2] * c[2] + matrix[y * 4 + 3] * c[3];
  }
  return RGBA(result[0], result[1], result[2], result[3]);
}
