
#ifndef IMAGE_H
#define IMAGE_H

#include <string>
#include "rgb.h"

/// Holds an image or texture.
class Image {
    public:
        Image(int h, int w);
	Image(int h, int w, double* data);
        Image(const std::string& filename);
	~Image();
	/// Sets a pixel
        void setRGB(int x, int y, RGB& color); 
	/// Gets a pixel
	RGB getRGB(int x, int y);
	/// Saves this image as a flat rgbrgb... stream
	void save(const std::string& filename);
	int getWidth() { return width; };
	int getHeight() { return height; };

    private:
	int height;
	int width;
	double *data;
};

#endif
