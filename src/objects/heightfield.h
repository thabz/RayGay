
#ifndef OBJECTS_HEIGHT_FIELD_H
#define OBJECTS_HEIGHT_FIELD_H

#include "objects/mesh.h"

class Texture;

/**
 * A heightfield.
 */
class HeightField : public Mesh {

    public:
	HeightField::HeightField(Texture* image, double height, double width, double depth, unsigned int width_divisions, unsigned int depth_divisions, const Material* material);
};

#endif
