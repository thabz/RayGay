
#ifndef IMAGE_MULTI_TEXTURE_H
#define IMAGE_MULTI_TEXTURE_H

#include "image/texture.h"
#include <string>
#include <vector>

class Image;

/**
 * A multitexture is one that is composed of a grid of images.
 */
class MultiTexture : public Texture {
public:
  MultiTexture(std::vector<std::string> filenames, uint32_t tiles_per_row,
               uint32_t memory_cached, const Vector2 &repeat_uv,
               Texture::InterpolationType it);

private:
  RGBA getRGB(int x, int y) const;
  Image *getTile(uint32_t index) const;

  mutable pthread_mutex_t mutex_loader;
  std::vector<std::string> filenames;
  mutable std::vector<Image *> images;
  mutable std::vector<int> positions;
  uint32_t tiles_per_row;
  uint32_t memory_cached;
  uint32_t tile_width;
  uint32_t tile_height;
  mutable uint32_t nextpos;
};

#endif