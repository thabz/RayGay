
#include "filters/gaussianblur.h"
#include "image/image.h"

using namespace std;

void GaussianBlur::apply(Image* image) {
    int height = image->getHeight();
    int width = image->getWidth();
    RGBA color;

    for(int y = 0; y < height; y++) {
	for(int x = 0; x < width; x++) {
	    color = image->getRGBA(x,y);
	    image->setRGBA(x,y,color.toGrayscale());
	}
    }
}
