
#include "math/vector.h"
#include "image/rgb.h"
#include <vector>

#include "boundingbox.h"

#define IRRADIANCE_OCTREE_MAX_NODES 16
#define IRRADIANCE_OCTREE_MAX_DEPTH 4

using namespace std;

/**
 * An irradiance cache as suggested by Greg Ward.
 *
 * @see http://radsite.lbl.gov/radiance/papers/erw92/paper.html
 * @see http://radsite.lbl.gov/radiance/papers/sg88/paper.html
 */
class IrradianceCache {

    public:
	/// Constructor
	IrradianceCache(const BoundingBox& bbox, double tolerance);

	/**
	 * Get an irradiance estimate. 
	 *
	 * This must be multiplied by the materials diffuse color.
	 *
	 * @param point the point we want diffuse irradiance estimate at
	 * @param normal the surface normal at point
	 * @param dest the irradiance estimate is returned here
	 * @return whether a irradiance estimate with a sufficiently high weight was found
	 */
         bool getEstimate(const Vector& point, const Vector& normal, RGB* dest) const;

	/**
	 * Insert an estimate into the cache.
	 *
	 * This inserts the result of a final gather into the irradiance cache.
	 *
	 * The harmonic mean distance \f$H\f$ is defined as
	 *
	 * \f[ H = \frac{n}{K} \f]
	 *
	 * where 
	 *
	 * \f[ K = \sum_{i=1}^{n}{\frac{1}{n_i}} \f]
	 *
	 * @param point the point where the final gather was done
	 * @param normal the surface normal at point
	 * @param irradiance the result of the final gather
	 * @param hmd the harmonic mean distance to the objects that was hit by final gather rays.
	 * 
	 */
	void putEstimate(const Vector& point, const Vector& normal, const RGB& irradiance, const double hmd);

    private:

	class CacheNode {
	    public:
		CacheNode(const Vector& point, const Vector &normal, const RGB& irradiance, double hmd, double a);


		double getWeight(const Vector& point, const Vector& normal) const;

		RGB getIrradiance() const { return RGB(irradiance[0],irradiance[1],irradiance[2]); };
		Vector getPoint() const { return Vector(point[0],point[1],point[2]); };
		Vector getNormal() const { return Vector(normal[0],normal[1],normal[2]); };

		/** 
		 * This is the squared distance from point where this 
		 * CacheNode doesn't matter anymore.
		 */
		float getSquaredRadius() const { return squared_radius; };
	    private:
		float point[3];
		float normal[3];
		float irradiance[3];
		float hmd;
		float squared_radius;
	};

	class HierarchyNode {
	    public:
		HierarchyNode(const BoundingBox& bbox, unsigned int depth);
		~HierarchyNode();
		void add(const CacheNode& node);
		void split();

		BoundingBox bbox;
		HierarchyNode* children[8];
		vector<CacheNode> cache_nodes;
		unsigned int depth;
		float length;
		bool isSplit;
	};

	double tolerance;
	double inv_tolerance;
	HierarchyNode* hierarchy_top;

	void traverseOctree(const HierarchyNode* const node, const Vector& point, vector<const CacheNode*>* result) const;
};



