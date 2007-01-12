
#include "imagefilters/specularbloom.h"
#include "imagefilters/gaussianblur.h"
#include "image/imageimpl.h"

SpecularBloom::SpecularBloom(double cutoff, double radius, double alpha) {
    this->cutoff = cutoff;
    this->radius = radius;
    this->alpha = alpha;
}

void SpecularBloom::apply(Image* image) {
    // TODO: Wouldn't a more lightweight <double,1> do the job too?
    Image* glow = new ImageImpl<double,4>(*image);

    RGBA black = RGBA(0,0,0,0);

    int w = image->getWidth();
    int h = image->getHeight();

    for(int x = 0; x < w; x++) {
	for(int y = 0; y < h; y++) {
	    RGBA col = glow->getRGBA(x,y);
	    if (col.brightness() > cutoff) {
		col = col.toGrayscale();
	    } else {
		col = black;
	    }
	    glow->setRGBA(x,y,col);
	}
    }

    // Clip colors
    glow->clipColors();

    GaussianBlur blur_filter = GaussianBlur(radius);
    blur_filter.apply(glow);

    //image->copy(glow);
    
    for(int x = 0; x < w; x++) {
	for(int y = 0; y < h; y++) {
	    RGBA col = image->getRGBA(x,y);
	    col += alpha * glow->getRGBA(x,y);
	    image->setRGBA(x,y,col);
	}
    }

    delete glow;
}
