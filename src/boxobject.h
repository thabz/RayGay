
#include <vector>
#include <iosfwd>

#include "object.h"
#include "vector.h"
#include "rgb.h"
#include "material.h"
#include "box.h"
#include "matrix.h"

class Intersection;
class Ray;
class Matrix;

class Boxobject : public object {

    public:
	Boxobject(Box b, Material m);
	Boxobject(Vector pos, double width, double height, double depth, Material m);

	virtual RGB getDiffuseColor(const Vector& p);
	virtual Material getMaterial();

	virtual void transform(const Matrix& m);
	virtual Vector normal(const Intersection& i);
	virtual bool intersects(Box& b);

	virtual bool onEdge(const Vector &p);
	virtual bool inside(const Vector &p);

    private:
	Box _box;
	Material _material;
	Matrix _transform;
	Matrix _invTransform;
	virtual Intersection _intersect(const Ray& ray);
};

