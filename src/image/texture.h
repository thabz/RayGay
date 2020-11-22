
#ifndef IMAGE_TEXTURE_H
#define IMAGE_TEXTURE_H

#include "image/rgb.h"
#include "image/rgba.h"
#include "math/vector2.h"

class Profiler;

/**
 * A texture is a wrapper around Image.
 *
 * A texture has support for getting wrapped and tiled interpolated texels.
 */
class Texture {

public:
  /// Interpolation types
  enum InterpolationType {
    INTERPOLATION_NONE,     ///< No interpolation. Fast.
    INTERPOLATION_BILINEAR, ///< Bilinear interpolation. Slow.
    INTERPOLATION_BICUBIC   ///< Bicubic interpolation. Slower.
  };

  /// Constructor
  Texture(const Vector2 &repeat_uv, InterpolationType it);
  /// Destructor
  virtual ~Texture();
  /// Get a texel
  RGB getTexel(double u, double v) const;
  /// Get a texel
  RGB getTexel(const Vector2 &uv) const;
  /// Returns width of image
  long getWidth() const;
  /// Returns height of image
  long getHeight() const;
  /// Change interpolation type
  void setInpolationType(InterpolationType it) { interpolation_type = it; };

private:
  double scaleU(double u) const;
  double scaleV(double v) const;
  /// Return a non-interpolated pixel where u and v in [0,1]
  RGB getNormalTexel(double u, double v) const;
  /// Return a bicubic interpolated pixel where u and v in [0,1]
  RGB getBiCubicTexel(double u, double v) const;
  /// Return a bilinear interpolated pixel where u and v in [0,1]
  RGB getBiLinearTexel(double u, double v) const;
  /// Helper for getBiCubicTexel()
  double biCubicR(const double x) const;
  RGBA getRGBWrapped(int x, int y) const;
  virtual RGBA getRGB(int x, int y) const = 0;

  Vector2 repeat_uv;
  InterpolationType interpolation_type;
  static Profiler *profiler;

protected:
  long width, height;
};

#endif
