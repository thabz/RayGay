
#ifndef IMAGE_PNG_IO_H
#define IMAGE_PNG_IO_H

#include "imageio.h"

/**
 * A loader and saver for PNG image files.
 */
class PngIO : public ImageIO {
    public:
	void save(const Image* const image, const std::string& filename) const;
	Image* load(const std::string& filename);
};

#endif

