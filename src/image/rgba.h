
#ifndef IMAGE_RGBA_H
#define IMAGE_RGBA_H

#include "image/rgb.h"

/**
 * A color with an alpha channel.
 */
class RGBA : public RGB {
    public:
	/// Default constructor
	RGBA();
	/// Copy constructor
	RGBA(const RGBA& rgb);
	/// Constructor
	RGBA(const RGB& rgb);
	/// Constructor
	RGBA(double r, double g, double b, double a);
	/// Get alpha value
	double a() const { return alpha; };

	/// Scale RGBA
	RGBA operator*(double c) const;
	/// Add another RGBA
	RGBA operator+(const RGBA& c) const;

    private:
	double alpha;
};

inline
RGBA::RGBA() : RGB() {
    alpha = 1;
}

inline
RGBA::RGBA(const RGBA& rgba) : RGB(rgba.r(),rgba.g(),rgba.b()) {
    alpha = rgba.a();
}

inline
RGBA::RGBA(const RGB& rgb) : RGB(rgb) {
    alpha = 1;
}

inline
RGBA::RGBA(double r, double g, double b, double a) : RGB(r,g,b)	{
    alpha = a;
}

inline
RGBA RGBA::operator*(double c) const {
     return RGBA(r()*c,g()*c,b()*c,alpha*c);
}

inline
RGBA RGBA::operator+(const RGBA& c) const {
     return RGBA(r()+c.r(),g()+c.g(),b()+c.b(),a()+c.a());
}

#endif

