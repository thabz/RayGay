
#ifndef OBJECTS_SDF_SMOOTH_CSG_H
#define OBJECTS_SDF_SMOOTH_CSG_H

#include "objects/sdf_object.h"

class SDFSmoothUnion : public SDFObject {

public:
  SDFSmoothUnion(Solid *left, Solid *right, const double blendRadius,
                 const double accuracy, const Material *m);

protected:
  /// Constructor
  SceneObject *clone() const;
  void transform(const Matrix &m);
  AABox getBoundingBox() const;

  double signedDistance(const Vector &point) const;
  SceneObject *clone() const;

private:
  Solid *left;
  Solid *right;
  double blendRadius;
};

#endif
