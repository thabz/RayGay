
#include "objects/ellipsoid.h"
#include "boundingbox.h"

Ellipsoid::Ellipsoid(const Vector& center, const Vector& radii, Material* material) : Solid(material) {
    sphere = new Sphere(Vector(0,0,0),1,NULL);
    this->transform(Matrix::matrixScale(radii));
    this->transform(Matrix::matrixTranslate(center));
}

void Ellipsoid::transform(const Matrix& m) {
    Transformer::transform(m);
}

BoundingBox Ellipsoid::boundingBoundingBox() const {
    return bboxToWorld(sphere->boundingBoundingBox());
}

Intersection Ellipsoid::_fullIntersect(const Ray& world_ray, const double t) const {
    Ray ray = rayToObject(world_ray);
    Intersection local_i = sphere->fullIntersect(ray,t*ray.t_scale);
    return intersectionToWorld(local_i);
}

double Ellipsoid::_fastIntersect(const Ray& world_ray) const {
    Ray local_ray = rayToObject(world_ray);
    double res = sphere->fastIntersect(local_ray);
    return res / local_ray.t_scale;
    return (res > 0) ? res / local_ray.t_scale : -1;
}

SceneObject* Ellipsoid::clone() const {
    return new Ellipsoid(*this);
}

vector<Intersection> Ellipsoid::allIntersections(const Ray& world_ray) const {
    Ray local_ray = rayToObject(world_ray);
    vector<Intersection> result1 = sphere->allIntersections(local_ray);
    vector<Intersection> result2;
    for(unsigned int i = 0; i < result1.size(); i++) {
	Intersection is = intersectionToWorld(result1[i]);
	is.setT(is.getT() / local_ray.t_scale);
	result2.push_back(is);
    }
    return result2;
}

