
#ifndef COLOR_XYZ_H
#define COLOR_XYZ_H

/**
 * The CIE 1931 XYZ color space
 *
 * @see http://en.wikipedia.org/wiki/CIE_1931_color_space
 */
class XYZ {
public:
  Lab toLab(XYZ whitepoint) const;

private:
  double X, Y, Z;
};

#endif
