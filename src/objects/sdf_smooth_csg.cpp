#include "objects/sdf_smooth_csg.h"
#include "aabox.h"
#include "intersection.h"
#include "math/functions.h"
#include <cmath>

class Material;

SDFSmoothUnion::SDFSmoothUnion(Solid *left, Solid *right,
                               const double blendRadius, const double accuracy,
                               const Material *material)
    : SDFObject(accuracy, material) {
  this->left = left;
  this->right = right;
  this->blendRadius = blendRadius;
}

// https://iquilezles.org/www/articles/smin/smin.htm
double SDFSmoothUnion::signedDistance(const Vector &p) const {
  double k = blendRadius;
  double d1 = left->signedDistance(p);
  double d2 = left->signedDistance(p);
  double h = Math::clamp(0.5 + 0.5 * (d2 - d1) / k);
  return Math::mix(d2, d1, h) - k * h * (1.0 - h);
}

SceneObject *SDFSmoothUnion::clone() const {
  Solid *lhs = dynamic_cast<Solid *>(this->left->clone());
  Solid *rhs = dynamic_cast<Solid *>(this->right->clone());
  return new SDFSmoothUnion(lhs, rhs, this->blendRadius, this->accuracy,
                            this->getMaterial());
}

AABox SDFSmoothUnion::getBoundingBox() const {
  AABox rb = right->getBoundingBox();
  AABox lb = left->getBoundingBox();
  return AABox::doUnion(rb, lb);
}

void SDFSmoothUnion::transform(const Matrix &m) {
  right->transform(m);
  left->transform(m);
}
