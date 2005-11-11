
#include "bezierpatch.h"
#include "math/functions.h"

/**
 * Constructor for a Bézier patch.
 *
 * @param points A pointer to 16 Vectors
 * @param uResolution Number of quads across
 * @param vResolution Number of quads down
 * @param material The material to use
 */
BezierPatch::BezierPatch(
	const vector<Vector> &points, 
	const uint32_t uResolution, 
	const uint32_t vResolution, 
	const Material* material) : ParametrizedSurface(uResolution, vResolution, false, false, material) {
    for(uint32_t i = 0; i < 16; i++) {
	controlPoints[i] = points[i];
    }
}

const Vector& BezierPatch::getControlPoint(uint32_t i, uint32_t j) const {
    return controlPoints[i * 4 + j];
}

double B(uint32_t n, double u) {
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

Vector BezierPatch::eval(double u, double v) const {
    Vector result = Vector(0,0,0);
    for (uint32_t i = 0; i < 4; i++) {
	for (uint32_t j = 0; j < 4; j++) {
	    result += getControlPoint(i,j) * B(i,u) * B(j,v);
	}
    }
    return result;
}

