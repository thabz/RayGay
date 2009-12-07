
#include "objects/transformedinstance.h"
#include "ray.h"
#include "aabox.h"
#include "intersection.h"
#include "exception.h"

TransformedInstance::TransformedInstance(Object* object) : Object(object->getMaterial()) {
    this->object = object;
}

TransformedInstance::TransformedInstance(Object* object, Material* material) : Object(material) {
    this->object = object;
}

void TransformedInstance::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
    Ray local_ray = rayToObject(ray);
    object->fullIntersect(local_ray,t,result);
    intersectionToWorld(result);
}

double TransformedInstance::_fastIntersect(const Ray& ray) const {
    Ray local_ray = rayToObject(ray);
    return object->fastIntersect(local_ray);
}

AABox TransformedInstance::getBoundingBox() const {
    return bboxToWorld(object->getBoundingBox());
}

SceneObject* TransformedInstance::clone() const {
    return new TransformedInstance(object);
}

void TransformedInstance::transform(const Matrix& m) {
    Transformer::transform(m);
}

uint32_t TransformedInstance::maxIntersections() const {
    Solid* solid = dynamic_cast<Solid*>(object);
    if (solid == NULL) {
	throw_exception("Transformed instance is not a solid");
    }
    return solid->maxIntersections();
}

uint32_t TransformedInstance::allIntersections(const Ray& ray, Intersection* result) const {
    Solid* solid = dynamic_cast<Solid*>(object);
    if (solid == NULL) {
	throw_exception("Transformed instance is not a solid");
    }
    Ray local_ray = rayToObject(ray);
    return solid->allIntersections(local_ray, result);
}

bool TransformedInstance::inside(const Vector& p) const {
    Solid* solid = dynamic_cast<Solid*>(object);
    if (solid == NULL) {
	throw_exception("Transformed instance is not a solid");
    }
    return solid->inside(pointToObject(p));

}

AABox TransformedInstance::getContainedBox() const {
    Vector tiny = Vector(EPSILON,EPSILON,EPSILON);
    return AABox(-1 * tiny, tiny);
}
