
#ifndef OBJECTS_BLOB
#define OBJECTS_BLOB

#include "objects/isosurface.h"
#include <vector>

/**
 * A Blob is a collection of spheres with a power.
 * If each sphere \f$ S_i \f$ has the power \f$ P_i(x,y,z) \f$
 * this surface is the points \f$ (x,y,z) \f$ where
 *
 * \f[ \sum_{i} P_i(x,y,z) = k \f]
 *
 * for some constant \f$ k \f$.
 *
 * This defines an IsoSurface.
 * 
 * @see http://www.dcs.shef.ac.uk/graphics/publications/implicit/overview.ps
 */
class Blob : public IsoSurface {
    
    public:
	Blob(double surface_density, unsigned int steps, double accuracy);
	void addAtom(const Vector& center, double a, double b);
    
    protected:
	double evaluateFunction(const Vector& point) const;

    private:
	int atoms_num;
	vector<Vector> centers;
	vector<double> as;
	vector<double> bs;
};


#endif
