

#ifndef IMAGE_TGA_IO_H
#define IMAGE_TGA_IO_H

#include "imageio.h"

class TgaIO : public ImageIO {
    public:
	void save(const Image* const image, const std::string& filename) const;
	Image* load(const std::string& filename);
};

#endif
