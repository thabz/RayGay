
#include <cmath>
#include "objects/sdf_object.h"
#include "aabox.h"

class Material;

SDFObject::SDFObject(Solid* solid, double grow, uint32_t steps, double accuracy, Material* m) : IsoSurface(steps,accuracy,grow,m) {
    this->solid = solid;
}

double SDFObject::evaluateFunction(const Vector& v) const {
    return solid->signedDistance(v);
}

SceneObject* SDFObject::clone() const {
    SDFObject* result = new SDFObject(*this);
    return result;
}

AABox SDFObject::_getBoundingBox() const {
    return solid->getBoundingBox();
}

