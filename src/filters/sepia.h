#ifndef FILTER_SEPIA_H
#define FILTER_SEPIA_H

#include "filters/filter.h"

/**
 * A filter for sepia-toning an image.
 */
class Sepia : public Filter2D {
    public:
	Sepia(double amount);
	void apply(Image* image);

    private:
	double amount;
};

#endif

