
#include "objects/halfspace.h"

Halfspace::Halfspace(const Vector& point, const Vector& normal, const Material* material) : Solid(material)
{
    this->point = point;
    this->normal = normal;
}

void Halfspace::transform(const Matrix& m) 
{
    point = m * point;
    normal = m.extractRotation() * normal;
}

BoundingBox Halfspace::boundingBoundingBox() const 
{
}

SceneObject* Halfspace::clone() const
{
    return new Halfspace(*this);
}

double Halfspace::_fastIntersect(const Ray& ray) const 
{
}

Intersection Halfspace::_fullIntersect(const Ray& ray, const double t) const
{
}

int Halfspace::intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const 
{
}

void Halfspace::allIntersections(const Ray& ray, vector<Intersection>& result) const 
{
}

