
#include "filters/grayscale.h"
#include "image/image.h"
#include <cstdio>    

using namespace std;

void Grayscale::apply(Image* image) {
    cout << "Applying grayscale filter" << endl;
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
