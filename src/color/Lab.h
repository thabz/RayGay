
#ifndef COLOR_LAB_H
#define COLOR_LAB_H

#include "color/XYZ.h"

/**
 * The CIE 1976 L*a*b* color space.
 *
 * @see http://en.wikipedia.org/wiki/Lab_color_space
 */
class Lab {
public:
  XYZ toXYZ(XYZ whitepoint) const;
  RGB toRGB(double gamma = 2.2);

private:
  double L, a, b;
};

#endif
