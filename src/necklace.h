#ifndef NECKLACE_H
#define NECKLACE_H

#include <vector>
#include "material.h"
#include "intersection.h"
#include "objectcollection.h"

class Path;
class Material;
class Matrix;
class BoundingBox;
class object;
class SpaceSubdivider;

/**
 * An object consisting of Spheres sitting on a path.
 */
class Necklace : public ObjectCollection {

    public:
	Necklace(const Path& path, int num, double r, const Material& material);
	virtual ~Necklace() {};
	
	/// Transform this object
	void transform(const Matrix& m);

	/// Adds this or all subobjects to a space
	void addParts(SpaceSubdivider* space);

    private:
	std::vector<object*> objects;
	int _num;
};

#endif

