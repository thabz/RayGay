
#include "shaders/shader.h"

Vector Shader::refract(const Vector &I, const Vector &N, double ior) const {
  return I.refract(N, ior);
}
