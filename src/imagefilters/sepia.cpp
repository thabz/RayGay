
#include "imagefilters/sepia.h"
#include "image/image.h"
#include <cstdio>    

using namespace std;

Sepia::Sepia(double amount) {
    this->amount = amount;
}

void Sepia::apply(Image* image) {
    int height = image->getHeight();
    int width = image->getWidth();
    RGBA c;
    RGBA d;

    for(int y = 0; y < height; y++) {
	for(int x = 0; x < width; x++) {
	    c = image->getRGBA(x,y);
	    c = c.toGrayscale();
	    d = RGBA(c.r() + 2 * amount, c.g() + amount, c.b(), c.a());
	    image->setRGBA(x,y,d);
	}
    }
}
