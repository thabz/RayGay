
#ifndef SHADERS_SHADER_H
#define SHADERS_SHADER_H

#include "shaders/shaderinput.h"
#include "image/rgb.h"
#include "materials/material.h"

class Shader {
    public:
	Shader(Material* material);
	virtual ~Shader() {};
	virtual RGB shade(const ShaderInput& shaderInput) = 0;

    protected:
	Material* material;
	Vector bump(const ShaderInput& shaderInput);
};

#endif

