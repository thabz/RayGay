
#ifndef OBJECTS_VOLUME_H
#define OBJECTS_VOLUME_H

#include "object.h"

/**
 * A solid is anything that knows whether any point is inside or outside.
 *
 * When this is known, ray intersection can be done using Marching Cubes.
 */
class Volume : public Object {

    protected:
	/// Says whether a point in inside this volume.
	virtual bool inside(const Vector& point) const = 0;

	Intersection _intersect(const Ray& ray) const;
};

#endif

