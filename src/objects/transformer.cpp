
#include "objects/transformer.h"
#include "math/vector2.h"
#include "boundingbox.h"

Transformer::Transformer(const Material* m) : Object(m) {
}

Intersection Transformer::_intersect(const Ray& ray) const {
    Vector Rd = inverse_rotation * ray.getDirection();
    Vector Ro = inverse_transformation * ray.getOrigin();
    Ray local_ray = Ray(Ro,Rd,ray.getIndiceOfRefraction());
    Intersection inter = this->localIntersect(local_ray);
    if (inter.isIntersected()) {
	Vector point = transformation * inter.getPoint();
	// TODO: If scaling allowed, calculate a new t
	double t = inter.getT();
	Intersection result = Intersection(inter);
	result.setPoint(point);
	result.setT(t);
	return result;
    } else {
	return Intersection();
    }
}

double Transformer::_fastIntersect(const Ray& ray) const {
    return _intersect(ray).getT();
}

Intersection Transformer::_fullIntersect(const Ray& ray, const double t) const {
    return _intersect(ray);
}


void Transformer::transform(const Matrix& m) {
    transformation = transformation * m;
    inverse_transformation = transformation.inverse();
    rotation = transformation.extractRotation();
    inverse_rotation = rotation.inverse();
}

Vector2 Transformer::getUV(const Intersection& i) const {
    Vector point = inverse_transformation * i.getPoint();
    Intersection inter = Intersection(point,i.getT());
    return this->localGetUV(inter);
}

Vector Transformer::normal(const Intersection &i) const {
    Vector point = inverse_transformation * i.getPoint();
    Intersection inter = Intersection(i);
    inter.setPoint(point);
    inter.setT(i.getT());
    Vector normal = this->localNormal(inter);
    return rotation * normal;
}

BoundingBox Transformer::boundingBoundingBox() const {
    BoundingBox bbox = this->localBoundingBoundingBox();
    Vector* corners = bbox.getCorners();
    for(int i = 0; i < 8; i++) {
	corners[i] = transformation * corners[i];
    }
    bbox = BoundingBox::enclosure(corners,8);
    delete [] corners;
    return bbox;
}
