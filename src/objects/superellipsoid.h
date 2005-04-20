
#ifndef OBJECTS_SUPERELLIPSOID_H
#define OBJECTS_SUPERELLIPSOID_H

#include "objects/isosurface.h"

/**
 * A shape between a sphere and a box.
 *
 * \f[ f(x,y,z) = \left( |x|^{2/n_2} + |y|^{2/n_2} \right)^{n_2 / n_1} + |z|^{2/n_1} \f]
 * The surface is \f$ f(x,y,z) = 1 \f$ and the interior is \f$ f(x,y,z) < 1 \f$
 * 
 * @see http://astronomy.swin.edu.au/~pbourke/surfaces/superellipse/
 * @see http://mathworld.wolfram.com/Superellipsoid.html
 */
class SuperEllipsoid : public IsoSurface {

    public:
	/// Constructor
	SuperEllipsoid(double n1, double n2, unsigned int steps, double accuracy, Material* m);
	SceneObject* clone() const;

    protected:
	AABox _getBoundingBox() const;

    private:
	double evaluateFunction(const Vector& v) const;
	double n1,n2;
    
};

#endif
