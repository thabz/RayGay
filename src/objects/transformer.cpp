
#include "objects/transformer.h"
#include "intersection.h"

void Transformer::transform(const Matrix& m) {
    transformation = transformation * m;
    inverse_transformation = transformation.inverse();
    rotation = transformation.extractRotation();
    inverse_rotation = rotation.inverse();
}

Vector Transformer::pointToObject(const Vector& p) const {
    return inverse_transformation * p;
} 

Vector Transformer::dirToObject(const Vector& d) const {
    return inverse_rotation * d;
}
Vector Transformer::pointToWorld(const Vector &p) const {
    return transformation * p;
}

Vector Transformer::dirToWorld(const Vector& d) const {
    return rotation * d;
}

Ray Transformer::rayToObject(const Ray& ray) const {
    Vector o = inverse_transformation * ray.getOrigin();
    Vector d = inverse_rotation * ray.getDirection();
    double ior = ray.getIndiceOfRefraction();
    return Ray(o,d,ior);
}

Intersection Transformer::intersectionToWorld(const Intersection& i) const {
    Intersection result = Intersection(i);
    result.setNormal(rotation * i.getNormal());
    result.setPoint(transformation * i.getPoint());
    return result;
}

BoundingBox Transformer::bboxToWorld(const BoundingBox& bbox) const {
    Vector* corners = bbox.getCorners();
    for(int i = 0; i < 8; i++) {
	corners[i] = transformation * corners[i];
    }
    BoundingBox result = BoundingBox::enclosure(corners,8);
    delete [] corners;
    return result;
}

