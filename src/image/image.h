
#ifndef IMAGE_H
#define IMAGE_H

#include <string>
#include "rgb.h"
#include "rgba.h"

#define IMAGE_FLOAT float

class Vector2;

/// Holds an image or texture.
class Image {
    public:
	/// Constructs an empty image
        Image(long h, long w);
	
	/// Constructs an image from rgbrgbrgb... data
	Image(long h, long w, IMAGE_FLOAT* data);
	
	// Constructs an image from a file
	Image(const std::string& filename);

	// Copy constructor
	Image(const Image& image);

	/// Destructor
	~Image();
	/// Sets a pixel
        void setRGBA(int x, int y, const RGBA& color); 
	/// Sets a pixel
        void setRGBA(const Vector2& p, const RGBA& color); 
	/// Gets a pixel
	RGBA getRGBA(const int x, const int y) const { 
	    IMAGE_FLOAT* p = &data[4*(y*width + x)];
	    return RGBA(*(p++),*(p++),*(p++),*p);
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
	IMAGE_FLOAT *data;

};

#endif
