
#include "objects/transformedinstance.h"
#include "math/vector2.h"
#include "boundingbox.h"
#include "intersection.h"

TransformedInstance::TransformedInstance(Object* object) : Transformer(object->getMaterial()) {
    this->object = object;
}

TransformedInstance::TransformedInstance(Object* object, Material* material) : Transformer(material) {
    this->object = object;
}

Intersection TransformedInstance::localIntersect(const Ray& ray) const {
    double t = object->fastIntersect(ray);
    if (t > 0) {
	Intersection result = object->fullIntersect(ray,t);
	return result;
    } else {
	return Intersection();
    }
}

BoundingBox TransformedInstance::localBoundingBoundingBox() const {
    return object->boundingBoundingBox();
}

SceneObject* TransformedInstance::clone() const {
    return new TransformedInstance(*this);
}

