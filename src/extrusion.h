#ifndef EXTRUSION_H 
#define EXTRUSION_H

#include "mesh.h"

class Vector;
class Path;

/// A cylinder object
class Extrusion : public Mesh {

    public:
	/// Constructor
    	Extrusion(const Vector& begin, const Vector& end, double radius, unsigned int segments, Material m);

        Extrusion(const Path& path, double radius, unsigned int segments, unsigned int pieces, Material m);
};

#endif
