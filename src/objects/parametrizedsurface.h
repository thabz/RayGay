
#ifndef PARAMETRIZED_SURFACE_H
#define PARAMETRIZED_SURFACE_H

#include "mesh.h"
#include "math/matrix.h"
#include <vector>

/**
 * A parametrized surface is a mesh constructed from a function
 * \f$ F: [0,1][0,1] \rightarrow R^3 \f$.
 */
class ParametrizedSurface : public Mesh 
{
    public:
	/// Constructor
	ParametrizedSurface(uint uRes, uint vRes, bool uClose, bool vClose, const Material* m);
	void prepare();
	void transform(const Matrix& m);

    protected:
	virtual Vector eval(double u, double v) const = 0;

    private:
	uint uRes, vRes;
	bool uClose, vClose;
	Matrix trans;
};

#endif

