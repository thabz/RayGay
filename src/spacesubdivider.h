
#ifndef SPACESUBDIVIDER_H
#define SPACESUBDIVIDER_H

class Object;
class Ray;
class Intersection;


/// All spacesubdividers must extend this abstract class.
class SpaceSubdivider {

    public:
	/// Place a object in the hierarchy
	virtual void addObject(Object* obj) = 0; 
	
	/// Calculate an intersection with the hierarchy
	virtual bool intersect(const Ray& ray) const = 0; 
	
	/// Calculate an intersection with the hierarchy
	virtual bool intersectPrimary(const Ray& ray) const = 0;  
	
	/// Calculate an intersection with the hierarchy
	virtual bool intersectForShadow(const Ray& ray) const = 0; 

	/// Calculate an intersection with the hierarchy
	virtual bool intersectForShadow(const Ray& ray, const Object* hint) const = 0; 
	
	/// This gets called after all objects are added and before any intersection methods are called.
	virtual void prepare() = 0;

	/// The last successful Intersection found
	virtual Intersection* getLastIntersection() const = 0;

    protected:
	SpaceSubdivider() {};
};

#endif

