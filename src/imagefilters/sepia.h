#ifndef FILTER_SEPIA_H
#define FILTER_SEPIA_H

#include "imagefilters/imagefilter.h"

/**
 * A filter for sepia-toning an image.
 *
 * TODO: Consider generalizing this into a tint-filter.
 */
class Sepia : public ImageFilter {
    public:
	Sepia(double amount);
	void apply(Image* image);

    private:
	double amount;
};

#endif

