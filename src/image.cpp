

#include "image.h"
#include <cassert>
#include <Magick++.h>
#include <iostream>
#include <stdio.h>
#include <math.h>

using namespace std;

Image::Image(int w, int h) {
    height = h;
    width = w;
    data = new double[w*h*3];
}

Image::Image(int w, int h, double* dataPtr) {
    height = h;
    width = w;
    data = dataPtr;
}

Image::Image(const std::string& filename) {
    try {
	Magick::Image img;
	img.read(filename);
	width = img.columns();
	height = img.rows();
	assert(width > 0);
	assert(height > 0);
	data = new double[width*height*3];
	img.write(0,0,width,height,"RGB",Magick::DoublePixel,data);
    } catch (Magick::Exception& error) {
	cout << "Caught exception: " << error.what() << endl;
    }
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
/*
#define byte unsigned char
void Image::save(const std::string& filename) {
    byte* bytes = new byte[height*width*3];
    RGB color = RGB(0.0,0.0,0.0);
    for(int y = 0; y < height; y++) {
        for(int x = 0; x < width; x++) {
            color = getRGB(x,y);
	    bytes[3*(x + y * width) + 0] = (byte)(round(color.r()*255));
	    bytes[3*(x + y * width) + 1] = (byte)(round(color.g()*255));
	    bytes[3*(x + y * width) + 2] = (byte)(round(color.b()*255));
	}
    }
    FILE* outfile = fopen(filename.c_str(),"wb");
    if (outfile == NULL) {
	std::cout << "Error opening " << filename;
	delete [] bytes;
	return;
    }
    int bytes_saved = fwrite(bytes,sizeof(byte),width*height*3,outfile);
    std::cout << bytes_saved << " bytes written" << std::endl;
    if (bytes_saved < width*height*3) 
	std::cout << "Error saving file" << std::endl;
    fclose(outfile);
    delete [] bytes;
}
*/

void Image::save(const std::string& filename) {
    Magick::Image img = Magick::Image(width,height,"RGB",Magick::DoublePixel,data);
    img.write(filename);
}

void Image::test() {
    Image img = Image("earth.jpg");
    Image out = Image(400,200);
    RGB col;

    for(double x = 0; x < 400; x++) {
        for(double y = 0; y < 200; y++) {
	    col = img.getTexel(x / 400.0, y / 200.0);
	    out.setRGB(int(x),int(y),col);
	}
    }
    out.save("test.png");
}
