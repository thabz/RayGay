#ifndef NECKLACE_H
#define NECKLACE_H

#include <vector>
#include "intersection.h"
#include "objectgroup.h"

class Path;
class Material;
class Matrix;
class BoundingBox;
class object;
class KdTree;

/// An object consisting of Spheres sitting on a path.
class Necklace : public ObjectGroup {

    public:
	/// Constructor
	Necklace(const Path* path, int num, double r, const Material* material);
	
	/// Destructor
	virtual ~Necklace() {};
	
    private:
	int _num;
};

#endif

