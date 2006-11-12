
#ifndef OBJECTS_HALFSPACE_H
#define OBJECTS_HALFSPACE_H


class Intersection;
class Ray;
class Matrix;
class Vector2;

// TODO: Implement this CSG object
class Halfspace : public Solid {

    public:
	Halfspace(const Vector& point, const Vector& normal);
	void transform(const Matrix& m);
	AABox getBoundingBox() const;
	SceneObject* clone() const;
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
	int intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const;
	void allIntersections(const Ray& ray, vector<Intersection>& result) const;
        bool inside(const Vector& p) const;

    private:
	Vector point;
	Vector normal;
};

#endif

