
#ifndef IMAGE_H
#define IMAGE_H

#include <string>
#include "rgb.h"
#include "rgba.h"

class Vector2;

/// Holds an image or texture.
class Image {
    public:
	/// Constructs an empty image
        Image(long h, long w);
	/// Constructs an image from rgbrgbrgb... data
	Image(long h, long w, double* data);
	/// Destructor
	~Image();
	/// Sets a pixel
        void setRGBA(int x, int y, const RGBA& color); 
	/// Sets a pixel
        void setRGBA(const Vector2& p, const RGBA& color); 
	/// Gets a pixel
	RGBA getRGBA(int x, int y) const;
	/// Saves this image as an uncompressed tga-file
	void save(const std::string& filename) const;
	/// Returns width of image
	int getWidth() const { return width; };
	/// Returns height of image
	int getHeight() const { return height; };
	/// Return a pixel where u and v in [0,1]
	RGB getTexel(double u, double v) const;
	/// Return a bicubic interpolated pixel where u and v in [0,1]
	RGB getBiCubicTexel(double u, double v) const;

	/// Converts image to grayscale
	void grayscale();

	/// Load a tga-file (the caller must free the Image)
	static Image* load(const std::string& filename);

	/// Internal test 
	static void test();

    private:
	long height;
	long width;
	double *data;

	double biCubicR(double x) const;
	double biCubicP(double x) const;
	RGB getRGBWrapped(int x, int y) const;

};

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
#endif
