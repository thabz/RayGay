
#ifndef IMAGE_H
#define IMAGE_H

#include <string>
#include "rgb.h"
#include "rgba.h"
#include "allocator.h"

#define IMAGE_FLOAT double 

class Vector2;

// TODO: Set block_size = 16 as compiletime constant. Then the divides and modulus below can be
// done using shifts and and.

#define blockno(x,y) (((y)/block_size)*blocks_w+((x)/block_size))
#define xy2offset(x,y) (blockno(x,y)*block_size*block_size + (y%block_size)*block_size + (x%block_size))*4

/// Holds an image or texture.
class Image 
{
    public:
	/// Constructs an empty image
        Image(long h, long w, Allocator::model_t = Allocator::AUTO);
	
	// Constructs an image from a file
	Image(const std::string& filename, Allocator::model_t = Allocator::AUTO);

	// Copy constructor
	Image(const Image& image, Allocator::model_t = Allocator::AUTO);

	/// Destructor
	~Image();
	
	/// Sets a pixel
        void setRGBA(int x, int y, const RGBA& color); 
        
	/// Sets a pixel
        void setRGBA(const Vector2& p, const RGBA& color); 
        
	/// Gets a pixel
	RGBA getRGBA(const int x, const int y) const {
	    uint32_t offset = xy2offset(x,y);
	    return RGBA(data[offset+0],data[offset+1],data[offset+2],data[offset+3]);
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
	static Image* load(const std::string& filename,  Allocator::model_t = Allocator::AUTO);

        // Find whether we support the imageformat with the extension of the filename 
        static bool supportsFormat(const std::string& filename);
        
        // Fill image with color
        void clear(const RGBA& color);

    private:
	int height;
	int width;
	int block_size;
	int blocks_w;
	int blocks_h;
	IMAGE_FLOAT *data;

        // Size of image in number of IMAGE_FLOATS 
	uint32_t alloc_size;
};

#endif
