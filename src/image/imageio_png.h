

#ifndef IMAGE_PNG_IO_H
#define IMAGE_PNG_IO_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_PNG_H

#include "imageio.h"

/**
 * A loader and saver for PNG image files.
 */
class PngIO : public ImageIO {
public:
  void save(const Image *const image, FILE *fp) const;
  Image *load(const std::string &filename,
              Allocator::model_t = Allocator::AUTO);
};

#endif
#endif
