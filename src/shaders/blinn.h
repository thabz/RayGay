
#ifndef SHADERS_BLINN_H
#define SHADERS_BLINN_H

#include "shaders/shader.h"

class Blinn : public Shader {

public:
  Blinn(Material *material) : Shader(material){};
  RGB shade(const ShaderInput &shaderInput);
};

#endif
