#ifndef IMAGE_OPENEXR_IO_H
#define IMAGE_OPENEXR_IO_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_OPENEXR

#include "imageio.h"

/**
 * A loader and saver for OpenEXR image files.
 */
class OpenExrIO : public ImageIO {
public:
  void save(const Image *const image, const std::string &filename) const;
  void save(const Image *const image, FILE *file) const;
  Image *load(const std::string &filename,
              Allocator::model_t = Allocator::AUTO);
};

#endif
#endif
