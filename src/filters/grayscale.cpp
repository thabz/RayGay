
#include "filters/grayscale.h"
#include "image/image.h"

void Grayscale::apply(Image* image) {
    int height = image->getHeight();
    int width = image->getWidth();
    RGBA color;

    for(int y = 0; y < height; y++) {
	for(int x = 0; x < width; x++) {
	    color = image->getRGBA(x,y);
	    color.toGrayscale();
	    image->setRGBA(x,y,color);
	}
    }
}
