#ifndef FILTER_GAUSSIAN_BLUR_H
#define FILTER_GAUSSIAN_BLUR_H

#include "imagefilters/imagefilter.h"

/**
 * A filter for grayscaling an image.
 */
class GaussianBlur : public ImageFilter {
    public:
	GaussianBlur(double radius);
	void apply(Image* image);
	
    private:
	double radius;
};

#endif

