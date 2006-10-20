
#ifndef IMAGE_H
#define IMAGE_H

#include <string>
#include "rgb.h"
#include "rgba.h"

#define IMAGE_FLOAT double 

class Vector2;

#define xy2blockno(x,y) (((y)/block_size)*(blocks_w)+((x)/blocks_size))
#define xy2offset(x,y) ((xy2blockno(x,y)*blocks_size*blocks_size) + (y*block_size) + x)*sizeof(IMAGE_FLOAT)*4

/// Holds an image or texture.
class Image {
    public:
	/// Constructs an empty image
        Image(long h, long w, bool use_mmap = false);
	
	/// Constructs an image from rgbrgbrgb... data
	Image(long h, long w, IMAGE_FLOAT* data);
	
	// Constructs an image from a file
	Image(const std::string& filename, bool use_mmap = false);

	// Copy constructor
	Image(const Image& image, bool use_mmap = false);

	/// Destructor
	~Image();
	/// Sets a pixel
        void setRGBA(int x, int y, const RGBA& color); 
	/// Sets a pixel
        void setRGBA(const Vector2& p, const RGBA& color); 
	/// Gets a pixel
	RGBA getRGBA(const int x, const int y) const { 
	    IMAGE_FLOAT* p = &data[4*(y*width + x)];
	    return RGBA(p[0], p[1], p[2], p[3]);
	//    return RGBA(*(p++),*(p++),*(p++),*p);
	};
	/// Saves this image 
	void save(const std::string& filename);
	/// Returns width of image
	int getWidth() const { return width; };
	/// Returns height of image
	int getHeight() const { return height; };

	/// Converts image to grayscale
	void grayscale();

	/// Clip colors
	void clipColors();

	/// Copy the content of other into this image
	void copy(Image* other);

	/// Load a tga-file (the caller must free the Image)
	static Image* load(const std::string& filename);

    private:
	int height;
	int width;
	int block_size;
	int blocks_w;
	int blocks_h;
	IMAGE_FLOAT *data;
	bool use_mmap;

	int alloc_size;
	void calcBlocks(int h, int w);
};

#endif
