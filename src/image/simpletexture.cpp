#include "image/simpletexture.h"
#include "image/image.h"

/**
 * @param image The Image to wrap
 * @param repeat_uv The number of horizonal and vertical tiles
 * @param it The interpolation method to use when getting texels
 */
SimpleTexture::SimpleTexture(Image* image, const Vector2& repeat_uv, Texture::InterpolationType it) :
    Texture(repeat_uv,it) {
    this->image = image;
    this->width = image->getWidth();
    this->height = image->getHeight();
}

/**
 * The destructor deletes the wrapped Image.
 */
SimpleTexture::~SimpleTexture() {
    delete image;
}

RGBA SimpleTexture::getRGBWrapped(int x, int y) const {
    x %= width;
    y %= height;
    if (x < 0) x += width;
    if (y < 0) y += height;
    return image->getRGBA(x,y);
}

RGBA SimpleTexture::getRGB(int x, int y) const {
    return image->getRGBA(x,y);
}

