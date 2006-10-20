
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "image/image.h"
#include "image/imageio_tga.h"
#include "image/imageio_jpeg.h"
#include "image/imageio_png.h"
#include "image/imageio_hdri.h"
#include "image/imageio_darwin.h"
#include "exception.h"
#include <cassert>
#include <iostream>
#include <fstream>
#include <cmath>
#include <string>
#include <memory.h>
#include "math/vector2.h"

#define byte unsigned char
#undef VERBOSE

using namespace std;

// TODO: map the floats as blocks sized getpagesize()

Image::Image(long w, long h, bool use_mmap) : use_mmap(use_mmap) {
    height = h;
    width = w;
    data = new IMAGE_FLOAT[w*h*4];
}

Image::Image(long w, long h, IMAGE_FLOAT* dataPtr) {
    height = h;
    width = w;
    data = dataPtr;
}

Image::Image(const std::string& filename, bool use_mmap) : use_mmap(use_mmap) {
    Image* image = Image::load(filename);
    (*this) = (*image);
}

Image::Image(const Image& other, bool use_mmap) : use_mmap(use_mmap) {
    width = other.width;
    height = other.height;
    data = new IMAGE_FLOAT[width * height * 4];
    memcpy(data,other.data, height * width * sizeof(IMAGE_FLOAT) * 4);
}

void Image::calcBlocks(int h, int w) {
    block_size = 16;
    blocks_w = w / block_size;
    if (w % block_size != 0) blocks_w++;
    blocks_h = h / blocks_h;
    if (h % block_size != 0) blocks_h++;
    alloc_size = block_size * block_size * blocks_w * blocks_h * 4 * sizeof(IMAGE_FLOAT);
}

/**
 * Frees the image data
 */
Image::~Image() {
    delete [] data;
}

void Image::copy(Image* other) {
    if (other->getWidth() != this->getWidth() || 
	other->getHeight() != this->getHeight()) {
	throw_exception("Image-sizes must match.");
    }
    memcpy(data,other->data, height * width * sizeof(IMAGE_FLOAT) * 4);
}


void Image::setRGBA(int x, int y, const RGBA& c) {
    /*
    assert(0 <= x && x < width);
    assert(0 <= y && y < height);
    */
    uint32_t offset = (y*width + x)*4;

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


// Caller must delete the returned ImageIO object
ImageIO* getImageIO(const std::string& filename) {
    ImageIO* io;

    if (filename.find(".jpg") != string::npos) {
#ifdef OS_DARWIN
    io = new DarwinIO();
#elif HAVE_JPEGLIB_H
	io = new JpegIO();
#else    
	throw_exception("Support for JPEG-files is not compiled in.");
#endif	
    } else if (filename.find(".tga") != string::npos) {
	io = new TgaIO();
    } else if (filename.find(".png") != string::npos) {
#ifdef OS_DARWIN	
        io = new DarwinIO();
#elif HAVE_PNG_H
    	io = new PngIO();
#else
	throw_exception("Support for PNG-files is not compiled in.");
#endif	
    } else if (filename.find(".hdr") != string::npos) {
	io = new HdriIO();
#ifdef OS_DARWIN	
    } else if (filename.find(".jp2") != string::npos) {
    	io = new DarwinIO();
    } else if (filename.find(".tif") != string::npos) {
    	io = new DarwinIO();
    } else if (filename.find(".tiff") != string::npos) {
    	io = new DarwinIO();
#endif

    } else {
	throw_exception(filename + " has unknown fileformat.");
    }
    return io;
}


/**
 * Writes the image into a  24 bit uncompressed tga-file
 */
void Image::save(const std::string& filename) {
    clipColors();
    ImageIO* io = getImageIO(filename);
    io->save(this,filename);
    delete io;
}

// Clip RGBA values to be in [0,1]
void Image::clipColors() {
    uint32_t num = (uint32_t)(getWidth()*getHeight()*4);
    for(uint32_t i = 0; i < num; i++) {
	if (data[i] < 0) data[i] = 0;
	if (data[i] > 1) data[i] = 1;
    }
}

/**
 * Loads the image from a tga 24 og 32 bit uncompressed tga-file.
 */
Image* Image::load(const std::string& filename) {

    // Checking that file exists
    bool exists = false;
    fstream fin;
    fin.open(filename.c_str(),ios::in);
    if (fin.is_open()) {
	exists = true;
    }
    fin.close();
    if (!exists) {
	throw_exception("File " + filename + " not found.");
    }

    // Create a loader
    ImageIO* io = getImageIO(filename);

    // Read image data
    Image* image = io->load(filename);
    delete io;

#ifdef VERBOSE    
    cout << "Loaded " << filename << " " << image->getWidth() << "x" << image->getHeight() << endl;
#endif    
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

