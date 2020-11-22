
#include "transformer.h"
#include <iostream>

Transformer::Transformer() {
  transformed = false;
  scaled = false;
}

void Transformer::transform(const Matrix &m) {
  transformation = transformation * m;
  inverse_transformation = transformation.inverse();
  Matrix3 rotation = transformation.extractRotation();
  inverse_rotation = rotation.inverse();
  normal_transformation = inverse_rotation.transpose();
  transformed = !transformation.isIdentity();
  scaled = !rotation.isOrthogonal();
}

Vector Transformer::pointToObject(const Vector &p) const {
  if (!transformed)
    return p;

  return inverse_transformation * p;
}

Vector Transformer::dirToObject(const Vector &d) const {
  if (!transformed)
    return d;

  return inverse_rotation * d;
}

AABox Transformer::bboxToWorld(const AABox &bbox) const {
  if (!transformed)
    return bbox;

  Vector *corners = bbox.getCorners();
  for (int i = 0; i < 8; i++) {
    corners[i] = pointToWorld(corners[i]);
  }
  AABox result = AABox::enclosure(corners, 8);
  delete[] corners;
  return result;
}
