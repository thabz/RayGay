
#include "objects/solidbox.h"

SolidBox::SolidBox(const Vector corner1, const Vector corner2, const Material* mat) : Solid(mat) {
    bbox = BoundingBox(corner1,corner2);
}


bool SolidBox::inside(const Vector& point_world) const {
    Vector point_local = pointToObject(point_world);
    Vector _c1 = bbox.minimum();
    Vector _c2 = bbox.maximum();
    return point_local[0] > _c1[0] - EPSILON &&
           point_local[1] > _c1[1] - EPSILON &&
           point_local[2] > _c1[2] - EPSILON &&
           point_local[0] < _c2[0] + EPSILON &&
           point_local[1] < _c2[1] + EPSILON &&
           point_local[2] < _c2[2] + EPSILON;

 //   return bbox.insideOrTouching(point_local);
}

BoundingBox SolidBox::boundingBoundingBox() const {
    BoundingBox result = bboxToWorld(bbox);
    result.grow(20*EPSILON);
    return result;
}

SceneObject* SolidBox::clone() const {
    return new SolidBox(*this);
}

vector<Intersection> SolidBox::allIntersections(const Ray& world_ray) const {
    vector<Intersection> result;
    Ray local_ray = rayToObject(world_ray);
    Vector2 ts = bbox.intersect(local_ray);
    if (ts[0] > EPSILON) {
	Intersection i = fullIntersect(world_ray,ts[0]);
	i.isEntering(true);
	result.push_back(i);
    }
    if (ts[1] > EPSILON) {
	Intersection i = fullIntersect(world_ray,ts[1]);
	i.isEntering(false);
	result.push_back(i);
    }
    if (result.size() == 1) {
	result[0].isEntering(false);
    }
    return result;
}

void SolidBox::transform(const Matrix& m) {
    Transformer::transform(m);
}

double SolidBox::_fastIntersect(const Ray& world_ray) const {
    Ray local_ray = rayToObject(world_ray);
    Vector2 ts = bbox.intersect(local_ray);

    if (ts[1] < ts[0])
	return -1;

    if (ts[0] > EPSILON) {
	return ts[0];
    } else if (ts[1] > EPSILON) {
	return ts[1];
    } else {
	return -1;
    }
}

Intersection SolidBox::_fullIntersect(const Ray& ray, const double t) const {
    Vector point = pointToObject(ray.getPoint(t));
    Vector normal = Vector(0,0,0);
    if (IS_EQUAL(point[0],bbox.maximum()[0])) {
	normal[0] = 1.0;
    } else if (IS_EQUAL(point[1],bbox.maximum()[1])) {
	normal[1] = 1.0;
    } else if (IS_EQUAL(point[2],bbox.maximum()[2])) {
	normal[2] = 1.0;
    } else if (IS_EQUAL(point[0],bbox.minimum()[0])) {
	normal[0] = -1.0;
    } else if (IS_EQUAL(point[1],bbox.minimum()[1])) {
	normal[1] = -1.0;
    } else if (IS_EQUAL(point[2],bbox.minimum()[2])) {
	normal[2] = -1.0;
    }
    Intersection i = Intersection(point,t,normal,Vector2(0,0));
    return intersectionToWorld(i);
}

 
