

#include "image.h"
#include <cassert>
#include <iostream>
#include <math.h>
#include <memory.h>

#define byte unsigned char

using namespace std;

Image::Image(long w, long h) {
    height = h;
    width = w;
    data = new double[w*h*3];
}

Image::Image(long w, long h, double* dataPtr) {
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


void Image::setRGB(int x, int y, RGB& c) {
    assert(0 <= x && x < width);
    assert(0 <= y && y < height);

    data[(y*width + x)*3 + 0] = c.r();
    data[(y*width + x)*3 + 1] = c.g();
    data[(y*width + x)*3 + 2] = c.b();
}

RGB Image::getRGB(int x, int y) const {
    assert(0 <= x && x < width);
    assert(0 <= y && y < height);

    double *p = &data[3*(y*width + x)];
    RGB rgb = RGB(p[0],p[1],p[2]);
    return rgb;
}

RGB Image::getRGBWrapped(int x, int y) const {
    x = x % width;
    y = y % height;
    if (x < 0) x = width + x;
    if (y < 0) y = height + y;
    return getRGB(x,y);
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
    return getRGB(int(u*(width-1)),int(v*(height-1)));
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
    for(int m = -1; m <= 2; m++) {
	for(int n = -1; n <= 2; n++) {
	    result = result + getRGBWrapped(i+m,j+n) * biCubicR(m-dx) * biCubicR(dy-n);
	}
    }
    return result;
}

double Image::biCubicR(double x) const {
    double Pxp2 = biCubicP(x+2);
    double Pxp1 = biCubicP(x+1);
    double Px = biCubicP(x);
    double Pxm1 = biCubicP(x-1);
    return (Pxp2*Pxp2*Pxp2 - 4*Pxp1*Pxp1*Pxp1 + 6*Px*Px*Px - 4*Pxm1*Pxm1*Pxm1) / 6.0;
}

double Image::biCubicP(double x) const {
   return x > 0 ? x : 0; 
}


/**
 * Writes the image into a  24 bit uncompressed tga-file
 */
void Image::save(const std::string& filename) const {
    byte* bytes = new byte[height*width*3];
    byte Header[18];
    memset(&Header,0,18);

    Header[0] = 0;
    Header[1] = 0;
    Header[2] = 2;
    Header[3] = 0;
    Header[4] = 0;
    Header[5] = 0;
    Header[6] = 0;
    Header[7] = 0;
    Header[8] = 0;
    Header[9] = 0;
    Header[10] = 0;
    Header[11] = 0;
    Header[12] = width;
    Header[13] = ((long) width >> 8);
    Header[14] = height;
    Header[15] = ((long) height >> 8);
    Header[16] = 24;
    Header[17] = 0;

    
    RGB color = RGB(0.0,0.0,0.0);

    for(int y = 0; y < height ; y++) {
        for(int x = 0; x < width; x++) {
            color = getRGB(x,(height - 1) - y);
	    bytes[3*(x + y * width) + 0] = (byte)(round(color.b()*255));
	    bytes[3*(x + y * width) + 1] = (byte)(round(color.g()*255));
	    bytes[3*(x + y * width) + 2] = (byte)(round(color.r()*255));
	}
    }
    FILE* outfile = fopen(filename.c_str(),"wb");
    if (outfile == NULL) {
	std::cout << "Error opening " << filename;
	delete [] bytes;
	return;
    }
    fseek(outfile,0,0);
    fwrite(Header,1,18,outfile);
    fseek(outfile,18,0);
    int bytes_saved = fwrite(bytes,sizeof(byte),width*height*3,outfile);

    std::cout << bytes_saved << " bytes written" << std::endl;
    if (bytes_saved < width*height*3) 
	std::cout << "Error saving file" << std::endl;
    fclose(outfile);
    delete [] bytes;
}

/**
 * Loads the image from a tga 24 og 32 bit uncompressed tga-file.
 */
Image* Image::load(const std::string& filename) {

    FILE *Handle;
    byte Header[18];
    Handle = fopen(filename.c_str(), "rb");
    if(Handle == NULL) {
        cout << "Error opening " << filename << endl;
        exit(EXIT_FAILURE);
    }

    fseek(Handle, 0, 0);
    fread(Header, 1, 18, Handle);


    int bpp = int(Header[16]) / 8; // Bytes per pixel
        cout << "bpp: " << bpp << endl;
    if (Header[2] != 2 || bpp < 3 ) {
        cout << "Error reading " << filename << ": Only 24 or 32 bit noncompressed RGB is supported" << endl;
        exit(EXIT_FAILURE);
    }
    
    long width = ((long) Header[13] << 8) + Header[12];
    long height = ((long) Header[15] << 8) + Header[14];
    assert(width > 0);
    assert(height > 0);
    byte* bytes = new byte[width*height*bpp];

    fseek(Handle, 18, 0);
    fread(bytes, bpp, width*height, Handle);
    fclose(Handle);

    double* data = new double[width*height*3];
    for(int y = 0; y < height ; y++) {
        for(int x = 0; x < width; x++) {
	    long offset = ((height-1-y)*width + x)*bpp;
	    data[(y*width + x)*3 + 0] = bytes[offset+2] / double(255.0);
	    data[(y*width + x)*3 + 1] = bytes[offset+1] / double(255.0);
	    data[(y*width + x)*3 + 2] = bytes[offset+0] / double(255.0);
	}
    }

    delete [] bytes;
    cout << "Loaded " << filename << " " << width << "x" << height << endl;
    return new Image(width,height,data);
}

/**
 * Grayscale the image.
 * 
 * Using the formula from ITU-R Recommendation BT.709, "Basic Parameter Values for the Studio and for International Programme Exchange (1990) [formerly CCIR Rec. 709] 
 */
void Image::grayscale() {
    RGB col;
    RGB res;
    for(int y = 0; y < height ; y++) {
        for(int x = 0; x < width; x++) {
	    col = getRGB(x,y);
            double gray = 0.2125 * col.r() + 0.7154 * col.g() + 0.0721 * col.b(); 
	    res = RGB(gray,gray,gray);
	    setRGB(x,y,res);
	}
    }
}

void Image::test() {
}
