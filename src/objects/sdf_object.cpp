#include "objects/sdf_object.h"
#include "aabox.h"
#include <cmath>

class Material;

SDFObject::SDFObject(Solid *solid, double grow, uint32_t steps, double accuracy,
                     Material *m)
    : IsoSurface(steps, accuracy, 0, m) {
  this->solid = solid;
  this->grow = grow;
}

double SDFObject::evaluateFunction(const Vector &p) const {
  return solid->signedDistance(p) - grow;
}

SceneObject *SDFObject::clone() const {
  SDFObject *result = new SDFObject(*this);
  return result;
}

AABox SDFObject::_getBoundingBox() const {
  AABox result = solid->getBoundingBox();
  result.grow(grow);
  return result;
}
