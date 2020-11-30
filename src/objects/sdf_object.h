
#ifndef OBJECTS_SDF_OBJECT_H
#define OBJECTS_SDF_OBJECT_H

#include "objects/isosurface.h"
#include "objects/solid.h"
#include <vector>

/**
 * A signed distance function object is solid that is grown.
 * Ie. a box gets a bigger rounded surface.
 */
class SDFObject : public IsoSurface {

public:
  /// Constructor
  SDFObject(Solid *solid, double grow, uint32_t steps, double accuracy,
            Material *m);
  SceneObject *clone() const;

protected:
  AABox _getBoundingBox() const;

private:
  double evaluateFunction(const Vector &v) const;
  Solid *solid;
  double grow;
};

#endif
