
#include "objects/transformedinstance.h"
#include "math/vector2.h"
#include "boundingbox.h"

TransformedInstance::TransformedInstance(Object* object) : Object(object->getMaterial()) {
    this->object = object;
}

TransformedInstance::TransformedInstance(Object* object, Material* material) : Object(material) {
    this->object = object;
}

void TransformedInstance::transform(const Matrix& m) {
    transformation = transformation * m;
    inverse_transformation = transformation.inverse();
    rotation = transformation.extractRotation();
    inverse_rotation = rotation.inverse();
}

Intersection TransformedInstance::_intersect(const Ray& ray) const {
    Vector Rd = inverse_rotation * ray.getDirection();
    Vector Ro = inverse_transformation * ray.getOrigin();
    Ray local_ray = Ray(Ro,Rd,ray.getIndiceOfRefraction());
    if (object->intersect(local_ray)) {
	Intersection* inter = object->getLastIntersection();
	Vector point = transformation * inter->getPoint();
	// TODO: If scaling allowed, calculate a new t
	double t = inter->getT();
	return Intersection(point,t);
    } else {
	return Intersection();
    }
}

Vector2 TransformedInstance::getUV(const Intersection& i) const {
    Vector point = inverse_transformation * i.getPoint();
    Intersection inter = Intersection(point,i.getT());
    return object->getUV(inter);
}

Vector TransformedInstance::normal(const Intersection &i) const {
    Vector point = inverse_transformation * i.getPoint();
    Intersection inter = Intersection(point,i.getT());
    Vector normal = object->normal(inter);
    return rotation * normal;
}

BoundingBox TransformedInstance::boundingBoundingBox() const {
    BoundingBox bbox = object->boundingBoundingBox();
    Vector* corners = bbox.getCorners();
    for(int i = 0; i < 8; i++) {
	corners[i] = transformation * corners[i];
    }
    bbox = BoundingBox::enclosure(corners,8);
    delete [] corners;
    return bbox;
}

SceneObject* TransformedInstance::clone() const {
    return new TransformedInstance(*this);
}

