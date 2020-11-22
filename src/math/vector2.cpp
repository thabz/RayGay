
#include "math/vector2.h"
#include "math/constants.h"

using namespace std;

ostream &operator<<(ostream &os, const Vector2 &x) {
  os << '(' << x[0] << ',' << x[1] << ")";
  return os;
}
