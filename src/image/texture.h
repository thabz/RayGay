
#ifndef IMAGE_TEXTURE_H
#define IMAGE_TEXTURE_H

#include "image/rgb.h"
#include "image/rgba.h"
#include "math/vector2.h"

class Image;

class Texture {

    public:
	enum InterpolationType {
	    INTERPOLATION_NONE,
	    INTERPOLATION_BILINEAR,
	    INTERPOLATION_BICUBIC
	};

	Texture(Image* image, const Vector2& repeat_uv, InterpolationType it);
	~Texture();
	RGB getTexel(double u, double v) const;
	RGB getTexel(const Vector2& uv) const;
	void grayscale();
	long getWidth() const;
	long getHeight() const;

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
	
	Image* image;
	long width, height;
	Vector2 repeat_uv;
	InterpolationType interpolation_type;
};

#endif
