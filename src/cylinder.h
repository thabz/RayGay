#ifndef CYLINDER_H
#define CYLINDER_H

#include "mesh.h"

class Vector;
class Path;

/// A cylinder object
class Cylinder : public Mesh {

    public:
	/// Constructor
    	Cylinder(const Vector& begin, const Vector& end, double radius, int segments, Material m);

        Cylinder(const Path& path, double radius, int segments, int pieces, Material m);
	/// Destructor
    	virtual ~Cylinder();

	/// Internal test
 	static void test();

};

#endif
