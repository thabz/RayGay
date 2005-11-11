
#ifndef OBJECTS_BLOB
#define OBJECTS_BLOB

#include <vector>

#include "objects/isosurface.h"
#include "aabox.h"
#include "space/generickdtree.h"

class BlobAtom 
{
    public:
	BlobAtom(double radius, double weight);
	virtual AABox getBoundingBox() const = 0;
	virtual double squaredDistToPoint(const Vector& point) const = 0;
	virtual int intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const = 0;
	
	double radius;
	double radius_squared;
	double weight;
};

class BlobAtomSphere : public BlobAtom {

    public:
	BlobAtomSphere(double radius, double weight, const Vector& center);
	AABox getBoundingBox() const;
	double squaredDistToPoint(const Vector& point) const;
	int intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const;

    private:
	Vector center;
};

class BlobAtomCylinder: public BlobAtom {

    public:
	BlobAtomCylinder(double radius, double weight, const Vector& from, const Vector& to);
	AABox getBoundingBox() const;
	double squaredDistToPoint(const Vector& point) const;
	int intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const;

    private:
	Vector from;
	Vector to;
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
	Blob(double iso, uint32_t steps, double accuracy, Material* material);
	~Blob();
	void addAtom(const Vector& center, double radius, double weight);
	void addAtom(const Vector& from, const Vector& to, double radius, double weight);
	void prepare();
    
	SceneObject* clone() const;

    protected:
	double evaluateFunction(const Vector& point) const;
	AABox _getBoundingBox() const;

    private:
	BlobTree* tree;
};

#endif
