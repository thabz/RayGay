#ifndef CYLINDER_H
#define CYLINDER_H

#include "mesh.h"

class Vector;

/// A cylinder object
class Cylinder : public Mesh {

    public:
	/// Constructor
    	Cylinder(const Vector& begin, const Vector& end, double radius, int segments, Material m);

	/// Destructor
    	virtual ~Cylinder();

	/// Internal test
 	static void test();

};

#endif
