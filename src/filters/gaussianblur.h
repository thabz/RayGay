#ifndef FILTER_GAUSSIAN_BLUR_H
#define FILTER_GAUSSIAN_BLUR_H

#include "filters/filter.h"

/**
 * A filter for grayscaling an image.
 */
class GaussianBlur : public Filter2D {
    public:
	void apply(Image* image);
};

#endif

