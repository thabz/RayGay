
#ifndef IMAGEFILTERS_FILTER_H
#define IMAGEFILTERS_FILTER_H

class Image;

/**
 * A filter for images
 */
class ImageFilter {

public:
  enum EdgeAction { NONE, WRAP_EDGES, CLAMP_EDGES };

  /// Apply the filter to an image
  virtual void apply(Image *image) = 0;

protected:
  /// Perform convolution of mask on image
  void applyMask(Image *image, double *mask, int w, int h,
                 EdgeAction edgeAction);
  void normalizeMask(double *mask, int w, int h);
  virtual ~ImageFilter(){};
};

#endif
