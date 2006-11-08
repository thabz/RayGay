
#include "image/imageio_tga.h"
#include "image/imageimpl.h"
#include "exception.h"
#include <cassert>
#include <iostream>
#include <string>
#include <cmath>

using namespace std;
#define byte unsigned char

/**
 * Writes the image into a  24 bit uncompressed tga-file
 */
void TgaIO::save(const Image* const image, const std::string& filename) const {
    int height = image->getHeight();
    int width = image->getWidth();

    byte* bytes = new byte[height*width*4];
    byte Header[18];
    ::memset(&Header,0,18);

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
    Header[16] = 32;
    Header[17] = 0;
    
    RGBA color;

    for(int y = 0; y < height ; y++) {
        for(int x = 0; x < width; x++) {
            color = image->getRGBA(x,(height - 1) - y);
	    bytes[4*(x + y * width) + 0] = (byte)(floor(color.b()*255 + 0.5));
	    bytes[4*(x + y * width) + 1] = (byte)(floor(color.g()*255 + 0.5));
	    bytes[4*(x + y * width) + 2] = (byte)(floor(color.r()*255 + 0.5));
	    bytes[4*(x + y * width) + 3] = (byte)(floor(color.a()*255 + 0.5));
	}
    }
    FILE* outfile = ::fopen(filename.c_str(),"wb");
    if (outfile == NULL) {
	delete [] bytes;
	throw_exception("Error opening " + filename);
    }
    ::fseek(outfile,0,0);
    ::fwrite(Header,1,18,outfile);
    ::fseek(outfile,18,0);
    int bytes_saved = ::fwrite(bytes,sizeof(byte),width*height*4,outfile);

    //std::cout << bytes_saved << " bytes written" << std::endl;
    if (bytes_saved < width*height*4) {
	throw_exception("Error saving file " + filename);
    }
    ::fclose(outfile);
    delete [] bytes;
}

RGBA readpixel(FILE* handle, int bpp) {
    byte data[4];
    ::fread(data,1,bpp,handle);
    RGBA result;
    switch(bpp) {
	case 3:
	    result = RGBA(data[2],data[1],data[0],1.0);
	    break;
	case 4:
	    result = RGBA(data[2],data[1],data[0],data[3]);
	    break;
	case 1:
	    result = RGBA(data[0],data[0],data[0],1.0);
	    break;
    }
    result *= double(1.0/255.0);
    return result;
}

/**
 * @param handle The open file handle
 * @param dest an array of width length for storing RGBA values
 * @param width pixels per line
 * @param rle whether the data is compressed
 * @param bpp bytes per pixel (3 or 4)
 */
void readscanline(FILE* handle, RGBA* dest, int width, int bpp, bool rle) {
    if (rle) {
	int pixels_read = 0;
	byte repcount;
	do {
	    ::fread(&repcount,1,1,handle);
	    int count = (int(repcount) & 127) + 1;
	    if (int(repcount) > 127) {
		// Process run-length packet
		RGBA color = readpixel(handle,bpp);
		for(int i = 0; i < count; i++) {
		    if (pixels_read + i >= width)
			continue; // Safety
		    dest[pixels_read + i] = color;
		}
	    } else {
		// Process raw packet
		for(int i = 0; i < count; i++) {
		    if (pixels_read + i >= width)
			continue; // Safety
		    dest[pixels_read + i] = readpixel(handle,bpp);
		}
	    }
	    pixels_read += count;
	} while (pixels_read < width);
    } else {
	// Read an uncompressed line
	for(int x = 0; x < width; x++) {
	    dest[x] = readpixel(handle,bpp);
	}
    }
}

/**
 * Loads the image from a tga 24 og 32 bit uncompressed tga-file.
 */
Image* TgaIO::load(const std::string& filename, Allocator::model_t model) {

    FILE *Handle;
    byte Header[18];
    Handle = ::fopen(filename.c_str(), "rb");
    if(Handle == NULL) {
        throw_exception("Error opening " + filename);
    }

    ::fseek(Handle, 0, 0);
    ::fread(Header, 1, 18, Handle);


    int bpp = int(Header[16]) / 8; // Bytes per pixel

    // Says whether the data is rle encoded
    bool rle = int(Header[2]) >> 3 == 1;
    //cout << "bpp: " << bpp << (rle ? " (rle)" : "") << endl;

    if (Header[1] != 0 || int(Header[2]) & 7 != 2 || bpp == 2 ) {
        throw_exception("Error opening " + filename + 
		        ": Only 8, 24 or 32 bit truecolor RGB is supported");
    }

    long width = ((long) Header[13] << 8) + Header[12];
    long height = ((long) Header[15] << 8) + Header[14];
    assert(width > 0);
    assert(height > 0);

    fseek(Handle, 18, 0);

    Image* image;
    if (bpp == 4) {
	image = new ImageImpl<uint8_t,4>(width,height,model);
    } else {
	image = new ImageImpl<uint8_t,3>(width,height,model);
    }

    RGBA* line = new RGBA[width];
    for(int y = 0; y < height ; y++) {
	readscanline(Handle,line,width,bpp,rle);
        for(int x = 0; x < width; x++) {
	    image->setRGBA(x,height-1-y,line[x]);
	}
    }
    ::fclose(Handle);
    return image;
}
