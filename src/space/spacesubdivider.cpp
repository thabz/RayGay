
#include "space/spacesubdivider.h"
#include "space/hitcache.h"
#include "intersection.h"
#include "objects/object.h"

SpaceSubdivider::SpaceSubdivider() {
    hitcache = new HitCache(8);
}

bool SpaceSubdivider::intersect(void* fromObject, const Ray& ray) {
    return false;
    /*
    if (fromObject == NULL) {
	return intersect(ray);
    } else {
	Object* object = hitcache->findEntry(fromObject);
	if (object == NULL) {
	    bool result = intersect(ray);
	    if (result) {
		Object* hitObject = getLastIntersection()->getObject();
		double t = getLastIntersection()->getT();
		hitcache->addEntry(fromObject,hitObject,t);
	    }
	    return result;
	} else {
	    if (object->intersect(ray)) {
		double max_t = object->getLastIntersection()->getT();
		return intersect(ray,double(0),max_t);
	    } else {
		bool result = intersect(ray);
		if (result) {
		    Object* hitObject = getLastIntersection()->getObject();
		    double t = getLastIntersection()->getT();
		    hitcache->addEntry(fromObject,hitObject,t);
		}
		return result;
	    }
	}
    }
    */
}
