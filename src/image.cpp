

#include "image.h"
#include <cassert>
#include <iostream>
#include <math.h>

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

RGB Image::getRGB(int x, int y) {
    assert(0 <= x && x < width);
    assert(0 <= y && y < height);

    double *p = &data[3*(y*width + x)];
    RGB rgb = RGB(p[0],p[1],p[2]);
    return rgb;
}

RGB Image::getTexel(double u, double v) {
    if (u < 0.0) u = 0.0;
    if (u > 1.0) u = 1.0;
    if (v < 0.0) v = 0.0;
    if (v > 1.0) v = 1.0;
    assert(width > 0);
    assert(height > 0);
    return getRGB(int(u*(width-1)),int(v*(height-1)));
}

#define byte unsigned char

void Image::save(const std::string& filename) {
    byte* bytes = new byte[height*width*3];
    byte Header[18];

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

Image* Image::load(const std::string& filename) {

    FILE *Handle;
    byte Header[18];
    Handle = fopen(filename.c_str(), "rb");
    if(Handle == NULL) {
        cout << "Error: can't open " << filename << endl;
        exit(EXIT_FAILURE);
    }

    fseek(Handle, 0, 0);
    fread(Header, 1, 18, Handle);

    
    long width = ((long) Header[13] << 8) + Header[12];
    long height = ((long) Header[15] << 8) + Header[14];
    byte* bytes = new byte[width*height*3];

    fseek(Handle, 18, 0);
    fread(bytes, 3, width*height, Handle);
    fclose(Handle);

    double* data = new double[width*height*3];
    for(int y = 0; y < height ; y++) {
        for(int x = 0; x < width; x++) {
	    long offset = ((height-1-y)*width + x)*3;
	    data[(y*width + x)*3 + 0] = bytes[offset+2] / double(255.0);
	    data[(y*width + x)*3 + 1] = bytes[offset+1] / double(255.0);
	    data[(y*width + x)*3 + 2] = bytes[offset+0] / double(255.0);
	}
    }

    delete [] bytes;
    return new Image(width,height,data);
}

void Image::test() {
}
