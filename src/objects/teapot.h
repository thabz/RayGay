
#ifndef TEAPOT_H
#define TEAPOT_H

#include "objectgroup.h"

class Vector;
class Material;

/**
 * A classic teapot object build from Bézier patches.
 *
 * See http://www.sjbaker.org/teapot/index.html
 */
class Teapot : public ObjectGroup {
    public:
	/// Constructor
	Teapot(const Vector& pos, double scale, unsigned int dimension, const Material* material);
};

#endif
