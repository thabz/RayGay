
#ifndef IMAGE_IO_H
#define IMAGE_IO_H

class Image;
#include <string>

class ImageIO {
    public:
	virtual void save(const Image* const image, const std::string& filename) const = 0;
	virtual Image* load(const std::string& filename) = 0;
};

#endif

