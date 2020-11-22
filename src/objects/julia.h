
#ifndef OBJECTS_JULIA_H
#define OBJECTS_JULIA_H

#include "math/quaternion.h"
#include "objects/isosurface.h"

/**
 * Implements a Julia quaternion fractal.
 *
 * @see http://astronomy.swin.edu.au/~pbourke/fractals/quatjulia/
 */
class Julia : public IsoSurface {
public:
  Julia(Quaternion c, uint32_t max_iter, uint32_t steps, double accuracy,
        double w_axis_offset, Material *mat);
  SceneObject *clone() const;

protected:
  double evaluateFunction(const Vector &point) const;
  AABox _getBoundingBox() const;
  Vector normal(const Vector &p) const;

private:
  Quaternion c;
  uint32_t max_iter;
  double w_axis_offset;
};

#endif
