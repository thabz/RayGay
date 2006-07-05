
#ifndef OBJECTS_JULIA_H
#define OBJECTS_JULIA_H

#include "objects/isosurface.h"
#include "math/quaternion.h"

/**
 * Implements a Julia quaternion fractal.
 *
 * @see http://astronomy.swin.edu.au/~pbourke/fractals/quatjulia/
 */
class Julia : public IsoSurface 
{

    protected:
	double evaluateFunction(const Vector& point) const;

    private:
	Quaternion c;

};

#endif


