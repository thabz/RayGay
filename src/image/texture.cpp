
#include "image/texture.h"
#include "profiler.h"

Profiler *Texture::profiler = NULL;

/////////////////////////////////////////////////////
// Public methods
/////////////////////////////////////////////////////

/**
 * @param repeat_uv The number of horizonal and vertical tiles
 * @param it The interpolation method to use when getting texels
 */
Texture::Texture(const Vector2 &repeat_uv, InterpolationType it) {
  this->repeat_uv = repeat_uv;
  this->interpolation_type = it;
  if (this->profiler == NULL) {
    this->profiler = Profiler::create("Texture lookup", "Rendering");
  }
}

Texture::~Texture() {}

/**
 * Returns a texel from the texture
 *
 * @param uv texel-coordinates
 */
RGB Texture::getTexel(const Vector2 &uv) const {
  return getTexel(uv[0], uv[1]);
}

/**
 * Returns a texel from the texture
 *
 * @param u first texel-coordinate
 * @param v second texel-coordinate
 */
RGB Texture::getTexel(double u, double v) const {
  profiler->start();
  switch (interpolation_type) {
  case INTERPOLATION_NONE:
    return getNormalTexel(u, v);
  case INTERPOLATION_BILINEAR:
    return getBiLinearTexel(u, v);
  case INTERPOLATION_BICUBIC:
    return getBiCubicTexel(u, v);
  default:
    throw "Unknown interpolationtype";
  }
  profiler->stop();
}

long Texture::getWidth() const { return width; }

long Texture::getHeight() const { return height; }

/////////////////////////////////////////////////////
// Private methods
/////////////////////////////////////////////////////

inline RGBA Texture::getRGBWrapped(int x, int y) const {
  x %= width;
  y %= height;
  if (x < 0)
    x += width;
  if (y < 0)
    y += height;
  return getRGB(x, y);
}

#define tile(x) ((x) < 0 ? 1.0 - fmod(x, 1) : fmod(x, 1))

inline double Texture::scaleU(double u) const {
  u *= repeat_uv[0];
  return tile(u);
}

inline double Texture::scaleV(double v) const {
  v *= repeat_uv[1];
  return tile(v);
}

RGB Texture::getNormalTexel(double u, double v) const {
  u = scaleU(u);
  v = scaleV(v);
  return getRGBWrapped(int(u * width), int(v * height));
}

#define BiCubicP(x) (x) > 0 ? (x) : 0

inline double Texture::biCubicR(const double x) const {
  double Pxp2, Pxp1, Px, Pxm1;

  if (x > 0) {
    Pxm1 = BiCubicP(x - 1);
    Px = x;
    Pxp1 = x + 1;
    Pxp2 = x + 2;
    return (Pxp2 * Pxp2 * Pxp2 - 4 * Pxp1 * Pxp1 * Pxp1 + 6 * Px * Px * Px -
            4 * Pxm1 * Pxm1 * Pxm1);
  } else { /* x <= 0*/
    Pxp1 = BiCubicP(x + 1);
    Pxp2 = BiCubicP(x + 2);
    return (Pxp2 * Pxp2 * Pxp2 - 4 * Pxp1 * Pxp1 * Pxp1);
  }
}

#undef BiCubicP

/**
 * Get a bicubic interpolated texel.
 *
 * @see http://astronomy.swin.edu.au/~pbourke/colour/bicubic/
 */
RGB Texture::getBiCubicTexel(double u, double v) const {
  u = scaleU(u);
  v = scaleV(v);

  double x = u * width;
  double y = v * height;
  int i = int(x);
  int j = int(y);

  double dx = x - i;
  double dy = y - j;

  RGB result = RGB(0.0, 0.0, 0.0);

  int realx;
  int beginx = i - 1;
  beginx %= width;
  if (beginx < 0)
    beginx += width;

  int realy = j - 1;
  realy %= height;
  if (realy < 0)
    realy += height;

  for (int n = -1; n <= 2; n++) {
    if (++realy >= height)
      realy = 0;
    realx = beginx;
    for (int m = -1; m <= 2; m++) {
      if (++realx >= width)
        realx = 0;
      result += getRGB(realx, realy) * (biCubicR(m - dx) * biCubicR(dy - n));
    }
  }
  return result / 36.0;
}

/**
 * Get a bilinear interpolated texel.
 *
 * @see http://www.gamedev.net/reference/articles/article669.asp
 */
RGB Texture::getBiLinearTexel(double u, double v) const {
  u = scaleU(u);
  v = scaleV(v);

  double x = u * width;
  double y = v * height;
  int xi = int(x);
  int yi = int(y);
  double dx = x - xi;
  double dy = y - yi;

  double ul = (1.0 - dx) * (1.0 - dy);
  double ll = (1.0 - dx) * dy;
  double ur = dx * (1.0 - dy);
  double lr = dx * dy;

  if (xi < width - 1 && yi < height - 1) {
    return getRGB(xi, yi) * ul + getRGB(xi, yi + 1) * ll +
           getRGB(xi + 1, yi) * ur + getRGB(xi + 1, yi + 1) * lr;

  } else {
    return getRGBWrapped(xi, yi) * ul + getRGBWrapped(xi, yi + 1) * ll +
           getRGBWrapped(xi + 1, yi) * ur + getRGBWrapped(xi + 1, yi + 1) * lr;
  }
}
