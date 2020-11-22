
#include "math/quaternion.h"

std::ostream &operator<<(std::ostream &os, const Quaternion &q) {
  return os << "(" << q.a1 << "," << q.a2 << "," << q.a3 << "," << q.a4 << ")";
}

bool Quaternion::operator==(const Quaternion &b) const {
  return IS_EQUAL(a1, b.a1) && IS_EQUAL(a2, b.a2) && IS_EQUAL(a3, b.a3) &&
         IS_EQUAL(a4, b.a4);
}

bool Quaternion::operator!=(const Quaternion &b) const { return !(*this == b); }

/**
 * Converts this quaternion to the corresponding orthogonal rotation matrix.
 * The quaternion must have norm = 1. The resulting matrix for a
 * quaternion \f$ q = (a,b,c,d) \f$ when \f$ |q| = 1 \f$ is
 *
 * \f[ \left[ \begin{array}{ccc}
  a^2 + b^2 - c^2 - d^2 & 2bc - 2ad & 2ac + 2bd \\
  2ad + 2bc & a^2 - b^2 + c^2 - d^2 & 2cd - 2ab \\
  2bd - 2ac & 2ab + 2cd & a^2 - b^2 - c^2 + d^2 \end{array} \right] \f]
 *
 * @see http://en.wikipedia.org/wiki/Quaternions_and_spatial_rotation
 */
Matrix Quaternion::toMatrix() const {
  assert(IS_EQUAL(norm(), 1.0));
  Matrix m;
  double a = a1;
  double b = a2;
  double c = a3;
  double d = a4;
  double aa = a * a;
  double bb = b * b;
  double cc = c * c;
  double dd = d * d;
  double b2 = 2 * b;
  double d2 = 2 * d;
  double bc2 = b2 * c;
  double ad2 = d2 * a;
  double ac2 = 2 * a * c;
  double bd2 = b2 * d;
  double cd2 = d2 * c;
  double ab2 = b2 * a;
  m.set(0, 0, aa + bb - cc - dd);
  m.set(1, 0, bc2 - ad2);
  m.set(2, 0, ac2 - bd2);
  m.set(0, 1, ad2 + bc2);
  m.set(1, 1, aa - bb + cc - dd);
  m.set(2, 1, cd2 - ab2);
  m.set(0, 2, bd2 - ac2);
  m.set(1, 2, ab2 + cd2);
  m.set(2, 2, aa - bb - cc + dd);
  return m;
}
