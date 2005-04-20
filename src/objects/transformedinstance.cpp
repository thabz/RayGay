
#include "objects/transformedinstance.h"
#include "ray.h"
#include "aabox.h"
#include "intersection.h"

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
    return new TransformedInstance(*this);
}

void TransformedInstance::transform(const Matrix& m) {
    Transformer::transform(m);
}

