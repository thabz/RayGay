
#ifndef OBJECTS_BLOB
#define OBJECTS_BLOB

#include <vector>

#include "objects/isosurface.h"
#include "boundingbox.h"
#include "space/generickdtree.h"

class BlobAtom 
{
    public:
	BlobAtom(double radius, double weight, const Vector& center);
	BoundingBox boundingBoundingBox() const;
	int intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const;
	
	double radius;
	double radius_squared;
	double weight;
	Vector center;
};

class BlobTree : public GenericKdTree<BlobAtom>
{
    public:
	BlobTree();
	double eval(const Vector& point) const;
};


/**
 * A Blob is a collection of spheres with a power.
 * If each sphere \f$ S_i \f$ has the power \f$ P_i(x,y,z) \f$
 * this surface is the points \f$ (x,y,z) \f$ where
 *
 * \f[ \sum_{i} P_i(x,y,z) = k \f]
 *
 * for some constant \f$ k \f$.
 *
 * We use the field function attributed to the Wyvill bros. for their Soft Objects.
 *
 * \f[
 * P_i = \left\{
 * \begin{array}{cc}
 *   a_i(1 - \frac{4}{9}\frac{r_i^6}{R_i^6} + \frac{17}{9}\frac{r_i^4}{R_i^4} - \frac{22}{9}\frac{r_i^2}{R_i^2}) & r_i \leq R_i \\
 *   0 & r_i > R_i
 * \end{array}
 * \right.
 * \f]
 *
 * where \f$ r_i \f$ is the distance from \f$(x,y,z)\f$ to the center of \f$ S_i \f$, \f$ R_i \f$ is its radius of influence and \f$ a_i \f$ is it's weight.
 * 
 * @see http://astronomy.swin.edu.au/~pbourke/modelling/implicitsurf/
 * @see http://www.dcs.shef.ac.uk/graphics/publications/implicit/overview.ps
 */
class Blob : public IsoSurface 
{
    
    public:
	/// Constructor
	Blob(double iso, unsigned int steps, double accuracy, Material* material);
	~Blob();
	void addAtom(const Vector& center, double radius, double weight);
	void prepare();
    
	SceneObject* clone() const;

    protected:
	double evaluateFunction(const Vector& point) const;
	BoundingBox _boundingBoundingBox() const;

    private:
	BlobTree* tree;
};

#endif
