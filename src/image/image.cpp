

#include "image.h"
#include "image/imageio_tga.h"
#include "image/imageio_jpeg.h"
#include "image/imageio_png.h"
#include <cassert>
#include <iostream>
#include <cmath>
#include <memory.h>
#include "math/vector2.h"

#define byte unsigned char

using namespace std;

Image::Image(long w, long h) {
    height = h;
    width = w;
    data = new IMAGE_FLOAT[w*h*4];
}

Image::Image(long w, long h, IMAGE_FLOAT* dataPtr) {
    height = h;
    width = w;
    data = dataPtr;
}

/**
 * Frees the image data
 */
Image::~Image() {
    delete [] data;
}


void Image::setRGBA(int x, int y, const RGBA& c) {
    /*
    assert(0 <= x && x < width);
    assert(0 <= y && y < height);
    */
    int offset = (y*width + x)*4;

    data[offset++] = c.r();
    data[offset++] = c.g();
    data[offset++] = c.b();
    data[offset] = c.a();
}

void Image::setRGBA(const Vector2& p, const RGBA& c) {
    int x = int(p[0]);
    int y = int(p[1]);
    if (x >= 0 && x < width && y >= 0 && y < height) {
	setRGBA(x,y,c);
    }
}

/*
RGBA Image::getRGBA(int x, int y) const {
    assert(0 <= x && x < width);
    assert(0 <= y && y < height);
    return RGBA(p[0],p[1],p[2],p[3]);
}
*/
RGBA Image::getRGBWrapped(int x, int y) const {
    x %= width;
    y %= height;
    if (x < 0) x += width;
    if (y < 0) y += height;
    return getRGBA(x,y);
}

/**
 * Returns a pixel from the image.
 *
 * @param u a value in [0,1]
 * @param v a value in [0,1]
 */
RGB Image::getTexel(double u, double v) const {
    u -= int(u);
    v -= int(v);
    return getRGBA(int(u*(width-1)),int(v*(height-1)));
}

/**
 * Using http://astronomy.swin.edu.au/~pbourke/colour/bicubic/
 */
RGB Image::getBiCubicTexel(double u, double v) const {
    u -= int(u);
    v -= int(v);
    double x = u*(width-1);
    double y = v*(height-1);
    int i = int(x); int j = int(y);
    double dx = x-i; double dy = y-j;

    RGB result = RGB(0.0,0.0,0.0);
    for(int n = -1; n <= 2; n++) {
	for(int m = -1; m <= 2; m++) {
	    result = result + (getRGBWrapped(i+m,j+n) * biCubicR(m-dx) * biCubicR(dy-n));
	}
    }
    return result;
}

#define ONE_OVER_SIX double(0.166666666666) 

double Image::biCubicR(double x) const {
    double Pxp2 = biCubicP(x+2);
    double Pxp1 = biCubicP(x+1);
    double Px = biCubicP(x);
    double Pxm1 = biCubicP(x-1);
    return ONE_OVER_SIX * (Pxp2*Pxp2*Pxp2 - 4*Pxp1*Pxp1*Pxp1 + 6*Px*Px*Px - 4*Pxm1*Pxm1*Pxm1);
}

/*
double Image::biCubicP(double x) const {
   return x > 0 ? x : 0; 
}
*/

ImageIO* getImageIO(const std::string& filename) {
    ImageIO* io;

    if (filename.find(".jpg") != string::npos) {
	io = new JpegIO();
    } else if (filename.find(".tga") != string::npos) {
	io = new TgaIO();
    } else if (filename.find(".png") != string::npos) {
	io = new PngIO();
    } else {
	cout << "Unknown fileformat." << endl;
        exit(EXIT_FAILURE);
    }
    return io;
}


/**
 * Writes the image into a  24 bit uncompressed tga-file
 */
void Image::save(const std::string& filename) const {
    ImageIO* io = getImageIO(filename);
    io->save(this,filename);
}

/**
 * Loads the image from a tga 24 og 32 bit uncompressed tga-file.
 */
Image* Image::load(const std::string& filename) {
    ImageIO* io = getImageIO(filename);
    Image* image = io->load(filename);
    delete io;
    cout << "Loaded " << filename << " " << image->getWidth() << "x" << image->getHeight() << endl;
    return image;
}

/**
 * Grayscale the image.
 * 
 * Using the formula from ITU-R Recommendation BT.709, "Basic Parameter Values for the Studio and for International Programme Exchange (1990) [formerly CCIR Rec. 709] 
 */
void Image::grayscale() {
    RGBA col;
    for(int y = 0; y < height ; y++) {
        for(int x = 0; x < width; x++) {
	    col = getRGBA(x,y);
	    setRGBA(x,y,col.toGrayscale());
	}
    }
}

