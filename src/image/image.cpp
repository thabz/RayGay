
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
#include "allocator.h"

#undef IMAGE_VERBOSE

using namespace std;

Image::Image(long w, long h) {
    height = h;
    width = w;
}

Image::~Image() {
}

void Image::setRGBA(const Vector2& p, const RGBA& c) {
    uint32_t x = int(p[0]);
    uint32_t y = int(p[1]);
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

bool Image::supportsFormat(const std::string& filename) {
    try {
        ImageIO* io = getImageIO(filename);
        delete io;
        return true;    
    } catch (Exception e) {
        return false;            
    }        
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
    for(uint32_t x = 0; x < width; x++) {
        for(uint32_t y = 0; y < height; y++) {
            setRGBA(x,y,getRGBA(x,y).clamped());
        }    
    }
}

// Copy contents of source into this image
void Image::copy(const Image& source)
{
    if (source.getWidth() != getWidth() || source.getHeight() != getHeight()) {
        throw_exception("Image sizes doesn't match");
    }        
    for(uint32_t x = 0; x < width; x++) {
        for(uint32_t y = 0; y < height; y++) {
            setRGBA(x,y,source.getRGBA(x,y).clamped());
        }    
    }
}


/**
 * Loads the image from a tga 24 og 32 bit uncompressed tga-file.
 */
Image* Image::load(const std::string& filename, Allocator::model_t model) {

    // Checking that file exists
    fstream fin;
    fin.open(filename.c_str(),ios::in);
    if (!fin.is_open()) {
	throw_exception("File " + filename + " not found.");
    }
    fin.close();

    // Create a loader
    ImageIO* io = getImageIO(filename);

    // Read image data
    Image* image = io->load(filename, model);
    delete io;

#ifdef IMAGE_VERBOSE    
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
    for(uint32_t y = 0; y < height ; y++) {
        for(uint32_t x = 0; x < width; x++) {
	    setRGBA(x,y,getRGBA(x,y).toGrayscale());
	}
    }
}

void Image::clear(const RGBA& color) 
{
    for(uint32_t y = 0; y < height ; y++) {
        for(uint32_t x = 0; x < width; x++) {
           setRGBA(x,y,color);        
        }
    }
}
