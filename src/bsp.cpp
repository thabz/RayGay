
#include "bsp.h"
#include "object.h"
#include "ray.h"
#include "intersection.h"
#include "boundingbox.h"
#include "constants.h"

BSP::BSP() {
    cutplane_dimension = 0;
    cutplane_value = 0;
}

void BSP::addObject(object* obj) {
    objects.push_back(obj);
}

void BSP::prepare() {
    if (objects.size() > BSP_MAX) {
	// Find the cutplane_dimension and cutplane_value
	BoundingBox bbox = enclosure();
	cutplane_dimension = largestDimension(bbox);
	cutplane_value = (bbox.maximum()[cutplane_dimension] - 
                         bbox.minimum()[cutplane_dimension]) / 2.0;

	lower = new BSP();
	higher = new BSP();
	std::vector<object*> cutting;

	// Put all objects into lower- or higher_objects
	for(int i = 0; i < objects.size(); i++) {
	    object* obj = objects[i];
	    bbox = obj->boundingBoundingBox();
	    int cut_val = bbox.cutByPlane(cutplane_dimension, cutplane_value);
	    if (cut_val == -1) {
		lower->addObject(obj);
	    } else if (cut_val == 1) {
		higher->addObject(obj);
	    } else {
		lower->addObject(obj);
		higher->addObject(obj);
	    }
	}
	objects.clear();

	// Recursive prepare()
	lower->prepare();
	higher->prepare();
    }
}

Intersection BSP::intersect(const Ray& ray) const {
    
}

Intersection BSP::intersectForShadow(const Ray& ray) const {
}

Intersection BSP::intersectForShadow(const Ray& ray, const object* hint) const {
}



/*******************
 * Private stuff   *
 *******************/

Intersection BSP::intersect(const Ray& ray, double min_t, double max_t) const {
    //TODO: Use max_t somewhere
    Vector o;
    if (min_t == -1) {
        o = ray.origin;
    } else {
        o = ray.origin + min_t * ray.direction;
    }

    if (o[cutplane_dimension] < cutplane_value && 
	ray.direction[cutplane_dimension] <= 0) {
        return lower->intersect(ray,min_t,max_t);
    } else if (o[cutplane_dimension] > cutplane_value && 
	ray.direction[cutplane_dimension] >= 0) {
        return higher->intersect(ray,min_t,max_t);
    } else {
	// Ray intersects cutplane (TODO: Check for zero-division case)
	double intersect_t = (cutplane_value - ray.origin[cutplane_dimension]) / ray.direction[cutplane_dimension];
	if (intersect_t > max_t) intersect_t = max_t;
	if (intersect_t < min_t) intersect_t = min_t;
	Intersection low_intersection = lower->intersect(ray,min_t,intersect_t);
	Intersection high_intersection = higher->intersect(ray,intersect_t,max_t);
	if (low_intersection.t < high_intersection.t) {
	    return low_intersection;
	} else {
	    return high_intersection;
	}
    }
}

BoundingBox BSP::enclosure() const {
    BoundingBox result; 
    for(int i = 0; i < objects.size(); i++) {
        result = BoundingBox::doUnion(result,objects[i]->boundingBoundingBox());
    }
    return result;
}

int BSP::largestDimension(const BoundingBox& box) const {
    double x = (box.maximum())[0] - (box.minimum())[0];
    double y = (box.maximum())[1] - (box.minimum())[1];
    double z = (box.maximum())[2] - (box.minimum())[2];
    double max = MAX(x,MAX(y,z));
    if (IS_EQUAL(x,max)) {
	    return 0;
    } else if (IS_EQUAL(y,max)) {
	    return 1;
    } else if (IS_EQUAL(z,max)) {
	    return 2;
    }
    // Throw an exception
}
