
#ifndef RAYGAY_CONTOUR_H
#define RAYGAY_CONTOUR_H

#include "vector2.h"
#include <vector>

/**
 * A contour is a series of closed 2D paths consisting of linesegments and quadratic bezier-curves.
 */
class Contour {
    public:
        Contour();
        ~Contour();
        bool isInside(const Vector2& point) const;
        std::vector<double> rasterize(double x_min, double x_max, double y, double size = 1) const;
        
        std::vector<Vector2> coords;
        std::vector<bool> onCurve;
        
    private:
        static int intersect(double x_min, double y, const Vector2& a, const Vector2& b, double* result);
        static int intersect(double x_min, double y, const Vector2& p0, const Vector2& p1, const Vector2& p2, double* result);
};

#endif
