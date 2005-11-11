
#ifndef IMAGE_IO_H
#define IMAGE_IO_H

class Image;
#include <string>
#include <cstdio>

/**
 * Image loaders must implement this interface.
 */
class ImageIO {
    public:
	/// Save an image
	virtual void save(const Image* const image, const std::string& filename) const = 0;
	/// Load an image
	virtual Image* load(const std::string& filename) = 0;

	virtual ~ImageIO() {};
};

#endif

