
#ifndef SPACESUBDIVIDER_H
#define SPACESUBDIVIDER_H

class Object;
class Ray;
class Intersection;
class HitCache;

/// All spacesubdividers must extend this abstract class.
class SpaceSubdivider {

    public:
	/// Place a object in the hierarchy
	virtual void addObject(Object* obj) = 0; 
	
	/// Calculate an intersection with the hierarchy
	virtual bool intersect(const Ray& ray, Intersection* result) const = 0; 
	/// Calculate an intersection with the hierarchy
	virtual bool intersectPrimary(const Ray& ray, Intersection* result) const = 0;  
	
	/**
	 * Calculate an intersection with the hierarchy.
	 *
	 * Only intersections that happen within [0,max_t] are considered.
	 * Others are ignored.
	 * 
	 * @param ray The ray to use for intersection
	 * @param max_t a max distance along the ray
	 *
	 * @return the blocking object or NULL if no intersection
	 */
	virtual Object* intersectForShadow(const Ray& ray, double max_t) const = 0; 
	/// This gets called after all objects are added and before any intersection methods are called.
	virtual void prepare() = 0;

	/// Calculate an intersection with the hierarchy
	bool intersect(void* fromObject, const Ray& ray); 

    protected:
	/// Constructor
	SpaceSubdivider();
	
    private:
	HitCache* hitcache;

};

#endif

