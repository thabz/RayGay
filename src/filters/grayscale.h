#ifndef FILTER_GRAYSCALE_H
#define FILTER_GRAYSCALE_H

#include "filters/filter.h"

/**
 * A filter for grayscaling an image.
 */
class Grayscale : public Filter2D {
    public:
	void apply(Image* image);
};

#endif

