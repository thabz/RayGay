
#ifndef OBJECTS_TRANSFORMED_INSTANCE_H
#define OBJECTS_TRANSFORMED_INSTANCE_H

#include "transformer.h"
#include "objects/object.h"
#include "objects/solid.h"

/**
 * A transformed instance of an object.
 *
 * This allows the same object to be duplicated 
 * around the scene in different positions, with
 * only one instance being kept in memory.
 */
class TransformedInstance : public Object, public SolidInterface, public Transformer {

    public:
	TransformedInstance(Object* object);
	TransformedInstance(Object* object, Material* material);

	AABox getBoundingBox() const;
	SceneObject* clone() const;
	void transform(const Matrix& m);

        void fullIntersect(const Ray& ray, const double t, Intersection& result) const;

        bool inside(const Vector& p) const;
	uint32_t allIntersections(const Ray& ray, Intersection* result) const;
	uint32_t maxIntersections() const;
	AABox getContainedBox() const;

    private:
	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;

	Object* object;
};

inline
void TransformedInstance::fullIntersect(const Ray& ray, const double t, Intersection& result) const
{
    return _fullIntersect(ray,t,result);        
}

#endif

