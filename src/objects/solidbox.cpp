
#include "objects/solidbox.h"

SolidBox::SolidBox(const Vector corner1, const Vector corner2, const Material* mat) : Solid(mat) {
    bbox = AABox(corner1,corner2);
}

AABox SolidBox::getBoundingBox() const {
    AABox result = bboxToWorld(bbox);
    result.growPercentage(0.1);
    return result;
}

SceneObject* SolidBox::clone() const {
    return new SolidBox(*this);
}

void SolidBox::allIntersections(const Ray& world_ray, vector<Intersection>& result) const {
    Ray local_ray = rayToObject(world_ray);
    Vector2 ts = bbox.intersect(local_ray);
    ts[0] /= local_ray.t_scale;
    ts[1] /= local_ray.t_scale;
    
    if (ts[1] < ts[0])
	return;
	    
    if (ts[0] > EPSILON) {
	Intersection i;
	fullIntersect(world_ray,ts[0],i);
	i.isEntering(true);
	result.push_back(i);
    }
    if (ts[1] > EPSILON) {
	Intersection i;
	fullIntersect(world_ray,ts[1],i);
	i.isEntering(false);
	result.push_back(i);
    }
    if (result.size() == 1) {
	result[0].isEntering(false);
    }
}

void SolidBox::transform(const Matrix& m) {
    Transformer::transform(m);
}

double SolidBox::_fastIntersect(const Ray& world_ray) const {
    Ray local_ray = rayToObject(world_ray);
    Vector2 ts = bbox.intersect(local_ray);
    ts[0] /= local_ray.t_scale;
    ts[1] /= local_ray.t_scale;

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

// TODO: Find UV-coordinates.
void SolidBox::_fullIntersect(const Ray& world_ray, const double t, Intersection& result) const {
    Ray local_ray = rayToObject(world_ray);
    Vector point = local_ray.getPoint(t*local_ray.t_scale);
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
    result = Intersection(point,t,normal,Vector2(0,0));
    intersectionToWorld(result);
}

bool SolidBox::inside(const Vector &p) const 
{
    return bbox.inside(p);        
}



 
