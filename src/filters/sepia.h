#ifndef FILTER_SEPIA_H
#define FILTER_SEPIA_H

#include "filters/filter.h"

/**
 * A filter for sepia-toning an image.
 *
 * TODO: Consider generalizing this into a tint-filter.
 * TODO: Implement this using a 5x5 Color matrix applied to all pixels, as
 * described in:
 * http://msdn.microsoft.com/msdnmag/issues/05/01/NETMatters/default.aspx
 */
class Sepia : public Filter2D {
    public:
	Sepia(double amount);
	void apply(Image* image);

    private:
	double amount;
};

#endif

