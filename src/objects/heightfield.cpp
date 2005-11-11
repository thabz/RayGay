
#include "objects/heightfield.h"
#include "image/texture.h"
#include <cassert>

HeightField::HeightField(
	Texture* texture, 
	double height, 
	double width, 
	double depth, 
	uint32_t width_divisions, 
	uint32_t depth_divisions, 
	const Material* material) : ParametrizedSurface(width_divisions, depth_divisions, false, false, material) 
{
    this->height = height;
    this->width = width;
    this->depth = depth;
    this->texture = texture;
}

Vector HeightField::eval(double u, double v) const {
    double y = height * texture->getTexel(u,v).brightness();
    double x = (u - 0.5) * width;
    double z = (v - 0.5) * depth;
    return Vector(x,y,z);
}
