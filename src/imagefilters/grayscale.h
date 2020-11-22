#ifndef FILTER_GRAYSCALE_H
#define FILTER_GRAYSCALE_H

#include "imagefilters/imagefilter.h"

/**
 * A filter for grayscaling an image.
 */
class Grayscale : public ImageFilter {
public:
  void apply(Image *image);
};

#endif
