
#include "imagefilters/matrixfilter.h"
#include "image/image.h"

MatrixFilter::MatrixFilter(const ColorMatrix &matrix) { this->matrix = matrix; }

void MatrixFilter::apply(Image *image) {
  int image_width = image->getWidth();
  int image_height = image->getHeight();
  RGBA tmp;

  for (int y = 0; y < image_height; y++) {
    for (int x = 0; x < image_width; x++) {
      tmp = matrix * image->getRGBA(x, y);
      image->setRGBA(x, y, tmp);
    }
  }
}
