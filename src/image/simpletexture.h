
#include "image/texture.h"

class Image;

class SimpleTexture : public Texture {
public:
  SimpleTexture(Image *image, const Vector2 &repeat_uv,
                Texture::InterpolationType it);
  ~SimpleTexture();

private:
  RGBA getRGBWrapped(int x, int y) const;
  RGBA getRGB(int x, int y) const;

  Image *image;
};
