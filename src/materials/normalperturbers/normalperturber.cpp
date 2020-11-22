
#include "materials/normalperturbers/normalperturber.h"

void NormalPerturber::transform(const Matrix &m) { Transformer::transform(m); }

Vector NormalPerturber::perturb(const Vector &point,
                                const Vector &normal) const {
  Vector local_point = pointToObject(point);
  Vector local_normal = dirToObject(normal);
  Vector result = _perturb(local_point, local_normal);
  return normalToWorld(result);
}
