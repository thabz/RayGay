
#ifndef FILTER2D_H
#define FILTER2D_H

class Image;

/**
 * A filter for images
 */
class Filter2D {

    public:
	virtual ~Filter2D() {};
	virtual void apply(Image* image) = 0;
};

#endif
