
#include "math/vector2.h"
#include "math/constants.h"

bool Vector2::operator==(const Vector2& x) const {
    return IS_EQUAL(x[0],_vector[0]) &&
           IS_EQUAL(x[1],_vector[1]);
}
