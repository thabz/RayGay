
#include "imagefilters/imagefilter.h"
#include "imagefilters/colormatrix.h"
#include "image/imageimpl.h"

RGBA getMaskedPixelHV(Image* image, double* mask, int w, int h, int x, int y, ImageFilter::EdgeAction edgeAction) {

    int image_height = image->getHeight();
    int image_width = image->getWidth();

    RGBA result = RGBA(0.0,0.0,0.0,0.0);

    // TODO: Don't darken result when mask goes beyond image border. Use the edgeAction parameter.

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

RGBA getMaskedPixelH(Image* image, double* mask, int w, int x, int y, ImageFilter::EdgeAction edgeAction) 
{
    int image_width = image->getWidth();
    RGBA result = RGBA(0.0,0.0,0.0,0.0);
    int w2 = w / 2;
    int image_y = y;

    for(int m_x = -w2; m_x <= w2; m_x++) {
        int image_x = m_x + x;
        double m = mask[w2+m_x];
        
        if (image_x < 0) {
            if (edgeAction == ImageFilter::CLAMP_EDGES) {
                image_x = 0;   
            } else if (edgeAction == ImageFilter::WRAP_EDGES) {
                image_x = (image_x + image_width) % image_width;
            }
        }
        if (image_x >= image_width) {
            if (edgeAction == ImageFilter::CLAMP_EDGES) {
                image_x = image_width - 1;   
            } else if (edgeAction == ImageFilter::WRAP_EDGES) {
                image_x = image_x % image_width;
            }
        }
        
        if (image_x >= 0 && image_x < image_width) {
	    result += m * image->getRGBA(image_x, image_y);
	}
    }
    return result;
}

RGBA getMaskedPixelV(Image* image, double* mask, int h, int x, int y, ImageFilter::EdgeAction edgeAction) {

    int image_height = image->getHeight();

    RGBA result = RGBA(0.0,0.0,0.0,0.0);
    int h2 = h / 2;
    int image_x = x;
    for(int m_y = -h2; m_y <= h2; m_y++) {
        int image_y = m_y + y;
        double m = mask[h2+m_y];
        
        if (image_y < 0) {
            if (edgeAction == ImageFilter::CLAMP_EDGES) {
                image_y = 0;   
            } else if (edgeAction == ImageFilter::WRAP_EDGES) {
                image_y = (image_y + image_height) % image_height;
            }
        }
        if (image_y >= image_height) {
            if (edgeAction == ImageFilter::CLAMP_EDGES) {
                image_y = image_height - 1;   
            } else if (edgeAction == ImageFilter::WRAP_EDGES) {
                image_y = image_y % image_height;
            }
        }
        
        if (image_y >= 0 && image_y < image_height) {
	    result += m * image->getRGBA(image_x, image_y);
	}
    }
    return result;
}


void ImageFilter::applyMask(Image* image, double* mask, int w, int h, ImageFilter::EdgeAction edgeAction) {
    Image* result = new ImageImpl<double,4>(image->getWidth(),image->getHeight());
    RGBA tmp;

    int image_width = image->getWidth();
    int image_height = image->getHeight();

    for(int y = 0; y < image_height; y++) {
	for(int x = 0; x < image_width; x++) {
	    tmp = getMaskedPixelH(image, mask, w, x, y, edgeAction);
	    result->setRGBA(x,y,tmp);
	}
    }
    image->copy(*result);
    
    for(int x = 0; x < image_width; x++) {
        for(int y = 0; y < image_height; y++) {
	    tmp = getMaskedPixelV(image, mask, h, x, y, edgeAction);
	    result->setRGBA(x,y,tmp);
	}
    }
    image->copy(*result);

    delete result;
}

void ImageFilter::normalizeMask(double* mask, int w, int h) {
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

    
