
#ifndef OBJECTS_BLOB
#define OBJECTS_BLOB

#include "objects/isosurface.h"
#include "boundingbox.h"
#include "materials/material.h"
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
 * \f[
 * P_i = \left\{
 * \begin{array}{cc}
 *   x_1 & 0 \leq r_i \leq \frac{b_i}{3} \\
 *   x_2 & 1 \leq r_i \leq b_i \\
 *   x_3 & b_i \leq r_i
 * \end{array}
 * \right.
 * \f]
 *
 * where \f$ r_i \f$ is 
 * 
 * TODO: Use Soft Objects if they're faster. See Paul Bourkes site below.
 *
 * @see http://astronomy.swin.edu.au/~pbourke/modelling/implicitsurf/
 * @see http://www.dcs.shef.ac.uk/graphics/publications/implicit/overview.ps
 */
class Blob : public IsoSurface {
    
    public:
	/// Constructor
	Blob(double iso, unsigned int steps, double accuracy, Material material);
	void addAtom(const Vector& center, double radius, double weight);
	BoundingBox boundingBoundingBox() const { return bbox; };
    
	void transform(const Matrix& m);
	const Material& getMaterial() const;
	SceneObject* clone() const;

    protected:
	double evaluateFunction(const Vector& point) const;

    private:
	Material material;
	BoundingBox bbox;
	int atoms_num;
	vector<Vector> centers;
	vector<double> radii;
	vector<double> weights;
};


#endif
