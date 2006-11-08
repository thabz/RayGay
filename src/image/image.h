
#ifndef IMAGE_H
#define IMAGE_H

#include <string>
#include "rgb.h"
#include "rgba.h"
#include "allocator.h"

class Vector2;

/// Holds an image or texture.
class Image 
{
    public:
	/// Sets a pixel
        virtual void setRGBA(int x, int y, const RGBA& color) = 0; 
        
	/// Sets a pixel
        void setRGBA(const Vector2& p, const RGBA& color); 
        
	/// Gets a pixel
	virtual RGBA getRGBA(const int x, const int y) const = 0;
	
	/// Saves this image 
	void save(const std::string& filename);
	
	/// Returns width of image
	int getWidth() const { return width; };
	
	/// Returns height of image
	int getHeight() const { return height; };

        /// Copy contents of source into this imag
        void copy(const Image& source);
        
	/// Converts image to grayscale
	void grayscale();

	/// Clip colors
	void clipColors();

	/// Load a tga-file (the caller must free the Image)
	static Image* load(const std::string& filename,  Allocator::model_t = Allocator::AUTO);

        // Find whether we support the imageformat with the extension of the filename 
        static bool supportsFormat(const std::string& filename);
        
        // Fill image with color
        void clear(const RGBA& color);

	virtual ~Image();
             
    protected:
        Image(long h, long w);
	Image() {};
        
    private: 
        uint32_t width;	    
        uint32_t height;	    
};

#endif
