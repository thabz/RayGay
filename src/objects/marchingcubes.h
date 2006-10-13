
#ifndef OBJECTS_MARCHING_CUBES_H
#define OBJECTS_MARCHING_CUBES_H

#include "objects/mesh.h"
#include "objects/isosurface.h"

/**
 * Converts an isosurface into a trianglemesh using
 * the marching cubes algorithm.
 *
 * William E. Lorensen and Harvey E. Cline: "Marching Cubes: A High 
 * Resolution 3D Surface Construction Algorithm", Computer Graphics 
 * (Proceedings of SIGGRAPH '87), Vol. 21, No. 4, pp. 163-169.
 *
 * The patent on marching cubes has recently expired, and it is safe 
 * for the graphics community to use it now, since more than 20 years 
 * have passed from its filing date (June 5, 1985) according to the US 
 * patent office
 */
class MarchingCubes : public Mesh
{
    public:
	MarchingCubes(IsoSurface* isosurface, uint32_t subdivisions, bool adaptive);

    protected:
	void prepare();

    private:
	Vector refine(const Vector& a, const Vector& b);
	void handleCube(const Vector cubeverts[8], uint32_t cubeindex);
	
	IsoSurface* isosurface;
	uint32_t subdivisions;
	bool adaptive;
};

#endif
