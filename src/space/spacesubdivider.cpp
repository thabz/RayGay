
#include "space/spacesubdivider.h"
#include "space/hitcache.h"
#include "intersection.h"
#include "objects/object.h"

SpaceSubdivider::SpaceSubdivider() {
    hitcache = new HitCache(8);
}

bool SpaceSubdivider::intersect(const Ray& ray, Intersection* inter, void* fromObject) {
    return false;
    /*
    if (fromObject == NULL) {
	return intersect(ray,inter);
    } else {
	Object* object = hitcache->findEntry(fromObject);
	bool result;
	if (object == NULL) {
	    result = intersect(ray,inter);
	} else {
	    double t_obj = object->fastIntersect(ray);
	    if (t_obj > 0) {
		result = intersect(ray,inter,double(0),t_obj + 0.5);
	    } else {
		result = intersect(ray,inter);
		if (!result) {
		    hitcache->removeEntry(fromObject);
		}
	    }
	}
	if (result) {
	    Object* hitObject = inter->getObject();
	    double t = inter->getT();
	    hitcache->addEntry(fromObject,hitObject,t);
	}
	return result;
    }
    */
}
