
#include <limits>
#include "objects/halfspace.h"
#include "aabox.h"
#include "math/matrix.h"
#include "math/vector.h"

Halfspace::Halfspace(const Vector& normal, double d, const Material* material) : Solid(material)
{
    this->d = d;
    this->normal = normal.normalized();
}

Halfspace::Halfspace(const Vector& a, const Vector& b, const Vector& c, const Material* material) : Solid(material)
{
    this->normal = Vector::xProduct(b-a, c-a);
    this->normal.normalize();
    this->d = -(this->normal * a);
}

void Halfspace::transform(const Matrix& m) 
{
    normal = m.extractRotation() * normal;
}

AABox Halfspace::getBoundingBox() const 
{
    // Return the whole world if the normal is not axis-aligned.
    // Otherwise we return a huge boundingbox that only contains half the world.
    double maxi = numeric_limits<double>::max();
    double mini = numeric_limits<double>::min();
    Vector mi = Vector(mini,mini,mini);
    Vector ma = Vector(maxi,maxi,maxi);
    return AABox(mi,ma);
    
    if (IS_EQUAL(normal[0],1)) {
	ma[0] = d + EPSILON;
    } else if (IS_EQUAL(normal[0],-1)) {
	mi[0] = d - EPSILON;
    } else if (IS_EQUAL(normal[1],1)) {
	ma[1] = d + EPSILON;
    } else if (IS_EQUAL(normal[1],-1)) {
	mi[1] = d - EPSILON;
    } else if (IS_EQUAL(normal[2],1)) {
	ma[2] = d + EPSILON;
    } else if (IS_EQUAL(normal[2],-1)) {
	mi[2] = d - EPSILON;
    }
    
    return AABox(mi,ma);
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
	return -(d + normal * ray.getOrigin()) / denominator;
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
    return 0;
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
