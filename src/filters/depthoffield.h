
#ifndef FILTER_DEPTH_OF_FIELD
#define FILTER_DEPTH_OF_FIELD

#include "filter.h"

class Image;

/**
 * Creates a depth of field blur from a depthmap and an image
 */
class DepthOfField : public Filter2D {

    public:
	DepthOfField(Image* depth, double focus_depth, double near_radius, double far_radius);
	void apply(Image* image);

    private:
	RGB sampleCircle(int x, int y, double z, double radius, double tolerance, Image* image) const;

	Image* depth_buffer;
	double focus_depth;
	double near_radius;
	double far_radius;
	double exponent;
};

#endif

