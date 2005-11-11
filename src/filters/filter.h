
#ifndef FILTER2D_H
#define FILTER2D_H

class Image;

/**
 * A filter for images
 */
class Filter2D {

    public:
	/// Apply the filter to an image
	virtual void apply(Image* image) = 0;

    protected:
	/// Perform convolution of mask on image
	void applyMask(Image* image, double* mask, int w, int h);
	void normalizeMask(double* mask, int w, int h);
	virtual ~Filter2D() {};
};

#endif
