
#ifndef SHADERS_PLASTIC_H
#define SHADERS_PLASTIC_H

#include "shaders/shader.h"

class Plastic : public Shader {

public:
  Plastic(Material *material) : Shader(material){};
  RGB shade(const ShaderInput &shaderInput);
};

#endif
