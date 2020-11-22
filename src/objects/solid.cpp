
#include "objects/solid.h"
#include "aabox.h"

AABox Solid::getContainedBox() const {
  Vector tiny = Vector(EPSILON, EPSILON, EPSILON);
  return AABox(-1 * tiny, tiny);
}
