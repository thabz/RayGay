
#include "objects/ellipsoid.h"
#include "boundingbox.h"

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

BoundingBox Ellipsoid::boundingBoundingBox() const {
    return bboxToWorld(sphere->boundingBoundingBox());
}

Intersection Ellipsoid::_fullIntersect( const Ray& world_ray, const double t) const {
    Ray ray = rayToObject(world_ray);
    double new_t = t*ray.t_scale;
    Vector p = ray.getPoint(new_t);
    // The normalized normal at a surface point of a unit-sphere
    // is the same as the surface point itself.
    Intersection local_i = Intersection(p,new_t,p,Vector2(0,0));
    return intersectionToWorld(local_i);
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
	Intersection is = intersectionToWorld(result1[i]);
	is.setT(is.getT() / local_ray.t_scale);
	result2.push_back(is);
    }
}

