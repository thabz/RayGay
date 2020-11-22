
#include "imagefilters/gaussianblur.h"
#include "image/image.h"
#include <cassert>

using namespace std;

GaussianBlur::GaussianBlur(double radius) {
  assert(radius > 0);
  this->radius = radius;
}

void GaussianBlur::apply(Image *image) {

  int h = (int)radius;

  double mask[h + 1];

  int h2 = h / 2;

  for (int x = -h2; x <= h2; x++) {
    double u = double(x) / radius;
    u *= 4;
    double val = exp(-0.5 * u * u);
    mask[x + h2] = val;
  }

  normalizeMask(mask, h + 1, 1);

  applyMask(image, mask, h, h, ImageFilter::WRAP_EDGES);
}
