#ifndef OBJECTS_EXTRUSION_H 
#define OBJECTS_EXTRUSION_H

#include "objects/mesh.h"
#include "paths/path.h"
#include "paths/circle.h"

/// An extrusion object
class Extrusion : public Mesh {

    public:
	/// Constructor for a simple cylinder
    	Extrusion(const Vector& begin, const Vector& end, double radius, unsigned int segments, Material* m);

	/// Constructor
        Extrusion(const Path& path, double radius, unsigned int segments, unsigned int pieces, Material* m);
};

#endif
