#ifndef BOX_H
#define BOX_H

#include "mesh.h"

class Material;
class Vector;

/// A box
class Box : public Mesh {

    public:
	/// Constructs a box with extremities at corner1 and corner2
	Box(const Vector corner1, const Vector corner2, Material mat);
	/// Constructs a box 
	Box(const Vector center, double width, double height, double depth, Material m);
	/// Destructor
	~Box();
};

#endif


