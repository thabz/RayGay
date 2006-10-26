

#ifndef IMAGE_TGA_IO_H
#define IMAGE_TGA_IO_H

#include "imageio.h"

/**
 * A loader and saver for Targa image files.
 *
 * @see http://www.ludorg.net/amnesia/TGA_File_Format_Spec.html for format.
 */
class TgaIO : public ImageIO {
    public:
	void save(const Image* const image, const std::string& filename) const;
	Image* load(const std::string& filename, Allocator::model_t = Allocator::AUTO);

	/* Found on http://organicbit.com/closecombat/formats/tga.html 
	typedef struct {
	    byte  identsize;          // size of ID field that follows 18 byte header (0 usually)
	    byte  colourmaptype;      // type of colour map 0=none, 1=has palette
	    byte  imagetype;          // type of image 0=none,1=indexed,2=rgb,3=grey,+8=rle packed

	    short colourmapstart;     // first colour map entry in palette
	    short colourmaplength;    // number of colours in palette
	    byte  colourmapbits;      // number of bits per palette entry 15,16,24,32

	    short xstart;             // image x origin
	    short ystart;             // image y origin
	    short width;              // image width in pixels
	    short height;             // image height in pixels
	    byte  bits;               // image bits per pixel 8,16,24,32
	    byte  descriptor;         // image descriptor bits (vh flip bits)

	    // pixel data follows header

	} TGA_HEADER; */
};

#endif
