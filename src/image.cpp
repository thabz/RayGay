

#include "image.h"
#include <cassert>
#include <iostream>
//#include <gdkmm/pixbuf.h>
#include <stdio.h>
#include <math.h>

#define byte unsigned char

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
    // TODO: Read the file into data
}

Image::~Image() {
    delete data;
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

/*
void Image::save(const std::string& filename) {
    guint8 *bytes = new guint8[height*width*3];
    RGB color;
    for(int y = 0; y < height; y++) {
        for(int x = 0; x < width; x++) {
            color = getRGB(x,y);
	    bytes[x + y * width + 0] = (guint8)(color.r*255);
	    bytes[x + y * width + 1] = (guint8)(color.g*255);
	    bytes[x + y * width + 2] = (guint8)(color.b*255);
	}
    }
    g_type_init();
    Glib::RefPtr<Gdk::Pixbuf> pixbuf = Gdk::Pixbuf::create_from_data (bytes, Gdk::COLORSPACE_RGB , false, 8, width, height,0);
 //   pixbuf->save(filename,"png");
}

*/
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
	delete bytes;
	return;
    }
    int bytes_saved = fwrite(bytes,sizeof(byte),width*height*3,outfile);
    std::cout << bytes_saved << " bytes written" << std::endl;
    if (bytes_saved < width*height*3) 
	std::cout << "Error saving file" << std::endl;
    fclose(outfile);
    delete bytes;
}
