#ifndef FILTER_SPECULAR_BLOOM_H

#define FILTER_SPECULAR_BLOOM_H

#include "filters/filter.h"

/**
 * A filter applying specular bloom to an image.
 *
 * @see http://www.neilblevins.com/cg_education/specular_bloom/specular_bloom.htm
 */
class SpecularBloom: public Filter2D {
    public:
	void apply(Image* image);
};

#endif

