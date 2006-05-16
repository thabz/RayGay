#ifndef FILTER_SPECULAR_BLOOM_H

#define FILTER_SPECULAR_BLOOM_H

#include "imagefilters/imagefilter.h"

/**
 * A filter applying specular bloom to an image.
 *
 * @see http://www.neilblevins.com/cg_education/specular_bloom/specular_bloom.htm
 */
class SpecularBloom: public ImageFilter {
    public:
	SpecularBloom(double cutoff, double radius, double alpha);
	void apply(Image* image);
	
    private:
	double cutoff;
	double radius;
	double alpha;
};

#endif

