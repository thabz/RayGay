
#ifndef IMAGE_JPEG_IO_H
#define IMAGE_JPEG_IO_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_JPEGLIB_H

#include "imageio.h"

/**
 * A loader and saver for JPEG image files.
 */
class JpegIO : public ImageIO {
public:
  void save(const Image *const image, FILE *fp) const;
  Image *load(const std::string &filename,
              Allocator::model_t = Allocator::AUTO);
};

#endif
#endif
