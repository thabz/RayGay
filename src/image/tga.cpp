
#include "image/tga.h"
#include "image/image.h"
#include <cassert>
#include <iostream>
#include <string>

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
    Header[16] = 32;
    Header[17] = 0;

    
    RGBA color;

    for(int y = 0; y < height ; y++) {
        for(int x = 0; x < width; x++) {
            color = image->getRGBA(x,(height - 1) - y);
	    bytes[4*(x + y * width) + 0] = (byte)(round(color.b()*255));
	    bytes[4*(x + y * width) + 1] = (byte)(round(color.g()*255));
	    bytes[4*(x + y * width) + 2] = (byte)(round(color.r()*255));
	    bytes[4*(x + y * width) + 3] = (byte)(round(color.a()*255));
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
    int bytes_saved = fwrite(bytes,sizeof(byte),width*height*4,outfile);

    std::cout << bytes_saved << " bytes written" << std::endl;
    if (bytes_saved < width*height*4) 
	std::cout << "Error saving file" << std::endl;
    fclose(outfile);
    delete [] bytes;
}

/**
 * Loads the image from a tga 24 og 32 bit uncompressed tga-file.
 */
Image* TgaIO::load(const std::string& filename) {

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

    Image* image = new Image(width,height);
    for(int y = 0; y < height ; y++) {
        for(int x = 0; x < width; x++) {
	    long offset = ((height-1-y)*width + x)*bpp;
	    RGBA color = RGBA(bytes[offset+2] / 255.0,
		              bytes[offset+1] / 255.0,
		              bytes[offset+0] / 255.0,double(1));
	    image->setRGBA(x,y,color);
	}
    }

    delete [] bytes;
    cout << "Loaded " << filename << " " << width << "x" << height << endl;
    return image;
}
