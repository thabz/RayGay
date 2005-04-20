
#include "objects/ellipsoid.h"
#include "aabox.h"
#include "materials/material.h"

Ellipsoid::Ellipsoid(const Vector& center, const Vector& radii, Material* material) : Solid(material) {
    // The ellipsoid in object space is simply a unit-sphere
    // with its center at the origin.
    sphere = new Sphere(Vector(0,0,0),1,NULL);
    transform(Matrix::matrixScale(radii));
    transform(Matrix::matrixTranslate(center));
}

void Ellipsoid::transform(const Matrix& m) {
    Transformer::transform(m);
}

AABox Ellipsoid::getBoundingBox() const {
    return bboxToWorld(sphere->getBoundingBox());
}

void Ellipsoid::_fullIntersect( const Ray& world_ray, const double t, Intersection& result) const {
    Ray ray = rayToObject(world_ray);
    double new_t = t*ray.t_scale;
    Vector p = ray.getPoint(new_t);

    Vector2 uv;
    if (getMaterial() != NULL && getMaterial()->requiresUV()) {
	uv = sphere->getUV(p);
    } 

    // The normalized normal at a surface point of a unit-sphere
    // is the same as the surface point itself.
    result = Intersection(p,new_t,p,uv);
    intersectionToWorld(result);
}

double Ellipsoid::_fastIntersect(const Ray& world_ray) const {
    Ray local_ray = rayToObject(world_ray);
    double res = sphere->fastIntersect(local_ray);
    return res / local_ray.t_scale;
}

SceneObject* Ellipsoid::clone() const {
    return new Ellipsoid(*this);
}

void Ellipsoid::allIntersections(const Ray& world_ray, vector<Intersection>& result2) const {
    Ray local_ray = rayToObject(world_ray);
    vector<Intersection> result1;
    sphere->allIntersections(local_ray,result1);
    for(unsigned int i = 0; i < result1.size(); i++) {
	Intersection is = result1[i];
	intersectionToWorld(is);
	is.setT(is.getT() / local_ray.t_scale);
	result2.push_back(is);
    }
}

