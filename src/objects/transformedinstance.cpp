
#include "objects/transformedinstance.h"
#include "ray.h"
#include "boundingbox.h"
#include "intersection.h"

TransformedInstance::TransformedInstance(Object* object) : Object(object->getMaterial()) {
    this->object = object;
}

TransformedInstance::TransformedInstance(Object* object, Material* material) : Object(material) {
    this->object = object;
}

Intersection TransformedInstance::_fullIntersect(const Ray& ray, const double t) const {
    Ray local_ray = rayToObject(ray);
    Intersection local_intersection = object->fullIntersect(local_ray,t);
    return intersectionToWorld(local_intersection);
}

double TransformedInstance::_fastIntersect(const Ray& ray) const {
    Ray local_ray = rayToObject(ray);
    return object->fastIntersect(local_ray);
}

BoundingBox TransformedInstance::boundingBoundingBox() const {
    return bboxToWorld(object->boundingBoundingBox());
}

SceneObject* TransformedInstance::clone() const {
    return new TransformedInstance(*this);
}

void TransformedInstance::transform(const Matrix& m) {
    Transformer::transform(m);
}

