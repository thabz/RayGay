
#include <limits>
#include "objects/halfspace.h"
#include "aabox.h"
#include "math/matrix.h"
#include "math/vector.h"

Halfspace::Halfspace(const Vector& normal, double d, const Material* material) : Solid(material)
{
    this->d = d;
    this->normal = normal;
}

Halfspace::Halfspace(const Vector& a, const Vector& b, const Vector& c, const Material* material) : Solid(material)
{
    this->normal = Vector::xProduct(b-a, c-a);
    this->d = -(this->normal * a);
}

void Halfspace::transform(const Matrix& m) 
{
    normal = m.extractRotation() * normal;
}

AABox Halfspace::getBoundingBox() const 
{
    double maxi = numeric_limits<double>::max();
    double mini = numeric_limits<double>::min();
    // Return the whole world if the normal is not axis-aligned.
    // Otherwise we return a huge boundingbox that only contains half the world.
    // TODO: Implement me.
    return AABox();
}

SceneObject* Halfspace::clone() const
{
    return new Halfspace(normal, d, getMaterial());
}

// See http://en.wikipedia.org/wiki/Line-plane_intersection
double Halfspace::_fastIntersect(const Ray& ray) const 
{
    double denominator = normal * ray.getDirection(); 
    if (IS_ZERO(denominator)) {
	return -1;
    } else {
	return (-d - normal * ray.getOrigin()) / denominator;
    }

}

void Halfspace::_fullIntersect(const Ray& ray, double t, Intersection& i) const
{
    Vector p = ray.getPoint(t);
    Vector n = normal;
    Vector2 uv;
    i = Intersection(p,t,n,uv);
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
    double t = fastIntersect(ray);
    if (t > 0) {
	Intersection i;
	fullIntersect(ray,t,i);
	result.push_back(i);
    }
}

// See http://mathworld.wolfram.com/Plane.html
bool Halfspace::inside(const Vector& x) const 
{
    double D = normal * x + d;
    return D < 0;
}
