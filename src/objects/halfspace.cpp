
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

Halfspace::Halfspace(const Vector& normal, const Vector& point, const Material* material) : Solid(material)
{
    init(normal.normalized(), point);
}

Halfspace::Halfspace(const Vector& a, const Vector& b, const Vector& c, const Material* material) : Solid(material)
{
    init(Vector::xProduct(b-a, c-a), a);
}

void Halfspace::init(const Vector& normal, const Vector& point) {
    this->normal = normal.normalized();
    this->d = -(this->normal * point);
}

void Halfspace::transform(const Matrix& m) 
{
    // We transform by simply using the d and normal to find a point on the surface.
    // That point and the normal are transformed and then used to init a new halfspace.
    Vector point = -d * normal;
    point = m * point;
    normal = m.extractRotation() * normal;
    init(normal, point);
}

AABox Halfspace::getBoundingBox() const 
{
    // Return the whole world if the normal is not axis-aligned.
    // Otherwise we return a huge boundingbox that only contains 
    // half the world.
    Vector mi = Vector(-HUGE_DOUBLE, -HUGE_DOUBLE, -HUGE_DOUBLE);
    Vector ma = Vector(HUGE_DOUBLE, HUGE_DOUBLE, HUGE_DOUBLE);
    
#ifdef HALFSPACE_OPTIMIZED_AABOX
    if (IS_EQUAL(normal[0],1)) {
	ma[0] = -d + EPSILON;
    } else if (IS_EQUAL(normal[0],-1)) {
	mi[0] = d - EPSILON;
    } else if (IS_EQUAL(normal[1],1)) {
	ma[1] = -d + EPSILON;
    } else if (IS_EQUAL(normal[1],-1)) {
	mi[1] = d - EPSILON;
    } else if (IS_EQUAL(normal[2],1)) {
	ma[2] = -d + EPSILON;
    } else if (IS_EQUAL(normal[2],-1)) {
	mi[2] = d - EPSILON;
    }
#endif
    
    return AABox(mi,ma);
}

SceneObject* Halfspace::clone() const
{
    return new Halfspace(normal, d, getMaterial());
}

double Halfspace::_fastIntersect(const Ray& ray) const 
{
    // See http://en.wikipedia.org/wiki/Line-plane_intersection
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

bool Halfspace::canSelfshadow() const {
    return false;
}
