
#include "objects/halfspace.h"
#include "aabox.h"
#include "math/matrix.h"
#include "math/vector.h"

Halfspace::Halfspace(const Vector& point, const Vector& normal, const Material* material) : Solid(material)
{
    this->point = point;
    this->normal = normal;
}

Halfspace::Halfspace(const Vector& a, const Vector& b, const Vector& c, const Material* material) : Solid(material)
{
    this->point = a;
    this->normal = Vector::xProduct(b-a, c-a);
}

void Halfspace::transform(const Matrix& m) 
{
    point = m * point;
    normal = m.extractRotation() * normal;
}

AABox Halfspace::getBoundingBox() const 
{
    // Return the whole world if the normal is not axis-aligned.
    // Otherwise we return a huge boundingbox that only contains half the world.
    // TODO: Implement me.
    return AABox();
}

SceneObject* Halfspace::clone() const
{
    return new Halfspace(point,normal,getMaterial());
}

// See http://en.wikipedia.org/wiki/Line-plane_intersection
double Halfspace::_fastIntersect(const Ray& ray) const 
{
    // TODO: Implement me.
    return 0;
}

void Halfspace::_fullIntersect(const Ray& ray, double t, Intersection& i) const
{
    // TODO: Implement me.
}

int Halfspace::intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const 
{
    // If any of the voxel_bbox' corners are inside the halfspace,
    // we have an intersection.
    Vector corners[8];
    voxel_bbox.getCorners(corners);
    for(int i = 0; i < 8; i++) {
	if (this->inside(corners[i])) {
	    return 1;
	}
    }
    return -1;
}

void Halfspace::allIntersections(const Ray& ray, vector<Intersection>& result) const 
{
    // TODO: Implement me.
}

bool Halfspace::inside(const Vector& p) const
{
    // TODO: Implement me.
    return false;
}
