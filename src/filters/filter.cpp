
#include "filters/filter.h"
#include "image/image.h"

RGBA getMaskedPixel(Image* image, double* mask, int w, int h, int x, int y) {

    int image_height = image->getHeight();
    int image_width = image->getWidth();

    RGBA result = RGBA(0.0,0.0,0.0,0.0);

    // TODO: Don't darken result when mask goes beyond image border.

    for(int m_x = 0; m_x < w; m_x++) {
	for(int m_y = 0; m_y < w; m_y++) {
	    int image_x = (m_x - (w/2)) + x;
	    int image_y = (m_y - (h/2)) + y;
	    if (image_x > 0 && image_x < image_width &&
		image_y > 0 && image_y < image_height) {
		double m = mask[m_y * w + m_x];
		result += m * image->getRGBA(image_x,image_y);
	    }
	}
    }
    return result;
}

void Filter2D::applyMask(Image* image, double* mask, int w, int h) {
    Image* result = new Image(image->getWidth(),image->getHeight());
    RGBA tmp;

    int image_width = image->getWidth();
    int image_height = image->getHeight();

    for(int y = 0; y < image_height; y++) {
	for(int x = 0; x < image_width; x++) {
	    tmp = getMaskedPixel(image, mask, w, h, x, y);
	    result->setRGBA(x,y,tmp);
	}
    }

    image->copy(result);
    delete result;
}

void Filter2D::normalizeMask(double* mask, int w, int h) {
    // Find sum of mask
    double sum = 0;
    for(int i = 0; i < w * h; i++) {
	sum += mask[i];
    }
    // Normalize
    for(int i = 0; i < w * h; i++) {
	mask[i] /= sum;
    }
}
