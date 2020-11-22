
#ifndef SURFACE_OF_REVOLUTION_H
#define SURFACE_OF_REVOLUTION_H

#include "math/vector2.h"
#include "mesh.h"
#include <vector>

using namespace std;

/**
 * Surface of revolution.
 *
 * A surface created by rotating a 2D-function around the y-axis.
 */
class SurfaceOfRevolution : public Mesh {
public:
  /// Constructor
  SurfaceOfRevolution(const vector<Vector2> &points, uint32_t segments,
                      const Material *m);
};

#endif
