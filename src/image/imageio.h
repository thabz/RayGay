
#ifndef IMAGE_IO_H
#define IMAGE_IO_H

class Image;
#include "allocator.h"
#include <cstdio>
#include <string>

/**
 * Image loaders must implement this interface.
 */
class ImageIO {
public:
  /// Save an image
  virtual void save(const Image *const image,
                    const std::string &filename) const;
  /// Save an image
  virtual void save(const Image *const image, FILE *dest) const = 0;
  /// Load an image
  virtual Image *load(const std::string &filename,
                      Allocator::model_t = Allocator::AUTO) = 0;

  virtual ~ImageIO(){};
};

#endif
