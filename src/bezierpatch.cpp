
#include "bezierpatch.h"
#include "math/functions.h"

/**
 * Constructor for a Bézier patch.
 *
 * @param points A pointer to 16 Vectors
 * @param xResolution Number of quads across
 * @param yResolution Number of quads down
 */
BezierPatch::BezierPatch(Vector* points, const unsigned int xResolution, const unsigned int yResolution, const Material& material) : Mesh(Mesh::MESH_FLAT,material) {
 //   this->controlPoints = new Vector[16];
    for(unsigned int i = 0; i < 16; i++) {
	controlPoints[i] = points[i];
    }
    Vector2 uvs[4];
    Vector verts[4];

    for(unsigned int x = 0; x < xResolution; x++) {
	for(unsigned int y = 0; y < yResolution; y++) {
	    double u1 = double(x) / double(xResolution);
	    double u2 = double(x+1) / double(xResolution);
	    double v1 = double(y) / double(yResolution);
	    double v2 = double(y+1) / double(yResolution);
	    uvs[0] = Vector2(u1,v1);
	    uvs[1] = Vector2(u2,v1);
	    uvs[2] = Vector2(u2,v2);
	    uvs[3] = Vector2(u1,v2);
	    for(int i = 0; i < 4; i++) {
		verts[i] = getPoint(uvs[i]);
	    }
	    addQuad(verts,uvs);
	}
    }
}

const Vector& BezierPatch::getControlPoint(unsigned int i, unsigned int j) const {
    return controlPoints[i + j * 4];
}

double B(unsigned int n, double u) {
    double nu = 1 - u;
    switch(n) {
	case 0:
	    return nu * nu * nu;
	case 1:
	    return 3 * u * nu * nu;
	case 2:
	    return 3 * u * u  * nu;
	case 3:
	    return u * u * u;
	default: // Shouldn't be reached
	    return 0; 
    }
}

Vector BezierPatch::getPoint(const Vector2& c) const {
    const double u = c[0];
    const double v = c[1];
    Vector result = Vector(0,0,0);
    for (unsigned int i = 0; i < 4; i++) {
	for (unsigned int j = 0; j < 4; j++) {
	    result += getControlPoint(i,j) * B(i,u) * B(j,v);
	}
    }
    return result;
}

