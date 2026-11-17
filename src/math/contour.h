
#ifndef RAYGAY_CONTOUR_H
#define RAYGAY_CONTOUR_H

#include "vector2.h"
#include <vector>

/**
 * A contour is a series of closed 2D paths consisting of linesegments and
 * quadratic bezier-curves.
 */
struct Contour {
  std::vector<Vector2> coords;
  std::vector<bool> onCurve;
};

class Contours {
public:
  Contours();
  ~Contours();
  void transform(float a, float b, float c, float d, float e, float f);
  std::vector<Contour> contours;

  /// Test membership using the non-zero winding rule.
  bool isInside(const Vector2 &point, double x_max, double size) const;
  /// Return filled-span transitions at or to the right of x_min.
  /// x_max is retained for compatibility; the scanline extends to infinity.
  std::vector<double> rasterize(double x_min, double x_max, double y,
                                double size = 1) const;

};

#endif
