#ifndef OBJECTS_EXTRUSION_H 
#define OBJECTS_EXTRUSION_H

#include "objects/mesh.h"
#include "paths/path.h"
#include "paths/circle.h"

/// An extrusion object
class Extrusion : public Mesh {

    public:
	/// Constructor for a simple cylinder
    	Extrusion(const Vector& begin, const Vector& end, double radius, uint32_t segments, Material* m);

	/// Constructor
        Extrusion(const Path& path, double radius, uint32_t segments, uint32_t pieces, Material* m);

	/// Constructor for generic extrusion
	Extrusion(const Path& path, const Path& circle, uint32_t segments, uint32_t pieces, double twists, Material* m);

    private:
	void init(const Path& path, const Path& circle, uint32_t segments, uint32_t pieces, double twists);
};

#endif
