
#include "bezierpatch.h"
#include "math/functions.h"

/**
 * Constructor for a Bézier patch.
 *
 * @param points A pointer to 16 Vectors
 * @param xResolution Number of quads across
 * @param yResolution Number of quads down
 */
BezierPatch::BezierPatch(Vector* points, int xResolution, int yResolution, const Material& material) : Mesh(Mesh::MESH_FLAT,material) {
    this->controlPoints = new Vector[16];
    for(unsigned int i = 0; i < 16; i++) {
	this->controlPoints[i] = points[i];
    }
    // TODO: Insert the triangles into mesh
}

const Vector& BezierPatch::getControlPoint(unsigned int i, unsigned int j) const {
    return controlPoints[i + j * 4];
}

#define B(i,t) Math::bernsteinPolynomial(i,4,t)
Vector BezierPatch::getPoint(double u, double v) const {
    Vector result = Vector(0,0,0);
    for (unsigned int i = 0; i < 4; i++) {
	for (unsigned int j = 0; j < 4; j++) {
	    result += getControlPoint(i,j) * B(i,u) * B(j,v);
	}
    }
    return result;
}

