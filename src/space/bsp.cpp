
#include <cassert>
#include <iostream>
#include <list>

#include "bsp.h"
#include "objects/object.h"
#include "ray.h"
#include "intersection.h"
#include "boundingbox.h"
#include "stats.h"
#include "materials/material.h"

// For test
#include "objects/sphere.h"

Object* BSP::last_primary_intersected_object = NULL;

BSP::BSP() {
    cutplane_dimension = 0;
    cutplane_value = 0;
}

void BSP::addObject(Object* obj) {
 //   Stats::getUniqueInstance()->inc("BSP: Objects added");
    objects.push_back(obj);
}

void BSP::prepare() {
    if (objects.size() > BSP_MAX) {
	// Find the cutplane_dimension and cutplane_value
	BoundingBox bbox = enclosure();
	Vector best_measure = Vector(0,HUGE_DOUBLE,0);
	int best_dim = -1;
	double best_val = 1;
	for(int i = 0; i < 3; i++) {
	    double val = median(i);
	    Vector measure = measureSplit(i,val);
	    if (measure[1] < best_measure[1] &&
		measure[0] <  objects.size() &&
		measure[2] <  objects.size()) {
		best_dim = i;
		best_val = val;
		best_measure = measure;
	    }
	}
	if (best_dim != -1) {
	    cutplane_dimension = best_dim;
	    cutplane_value = best_val;
	} else {
	    cutplane_dimension = largestDimension(bbox);
	    cutplane_value = median(cutplane_dimension);
	}

	lower = new BSP();
	higher = new BSP();

	unsigned int size = objects.size();

	// Put all objects into lower- or higher_objects
	int l = 0; int m = 0; int h = 0;
	for(unsigned int i = 0; i < size; i++) {
	    Object* obj = objects[i];
	    BoundingBox bbox = obj->boundingBoundingBox();
	    int cut_val = bbox.cutByPlane(cutplane_dimension, cutplane_value);
	    if (cut_val == -1) {
		lower->objects.push_back(obj);
		l++;
	    } else if (cut_val == 1) {
		higher->objects.push_back(obj);
		h++;
	    } else {
		lower->objects.push_back(obj);
		higher->objects.push_back(obj);
		m++;
	    }
	}
	
	if (lower->objects.size() == size || higher->objects.size() == size) {
	    // Objects couldn't be subdivided
	    delete lower;
	    delete higher;
	} else {
	    objects.clear();
	    assert(objects.size() == 0);
	    // Recursive prepare()
	    lower->prepare();
	    higher->prepare();
	}
    }
}

inline
bool BSP::intersect(const Ray& ray) const {
    ray.lowest_t = HUGE_DOUBLE;
    return intersect(ray,0,HUGE_DOUBLE);
}

/**
 * Speed up version of intersect for primary rays only.
 *
 * The trick is that two adjacent primary rays often hit
 * the same object. We check if the current primary ray hit the same
 * object as the previous primary ray. In that case, we can 
 * find a max bound for the t-value of the current primary ray.
 *
 * Ie. the ray.lowest_t can be set to a smaller value than HUGE_DOUBLE.
 */
inline
bool BSP::intersectPrimary(const Ray& ray) const {

    if (last_primary_intersected_object != NULL &&
	    last_primary_intersected_object->intersect(ray)) {
	ray.lowest_t = last_primary_intersected_object->getLastIntersection()->getT();;
    } else {
	ray.lowest_t = HUGE_DOUBLE;
    }

    bool result = intersect(ray,double(0),HUGE_DOUBLE);

    if (result) {
	last_primary_intersected_object = getLastIntersection()->getObject();
    } else {
	last_primary_intersected_object = NULL;
    }
    return result;
}

inline
bool BSP::intersectForShadow(const Ray& ray, double max_t) const {
    return intersectForShadow(ray,double(0),max_t);
}

bool BSP::intersectForShadow(const Ray& ray, const Object* hint, double max_t) const {
    if (hint != NULL && (!hint->getMaterial()->noShadow()) && hint->intersect(ray) && hint->getLastIntersection()->getT() <= max_t) {
	last_intersection = hint->getLastIntersection();
	return true;
    } else {
	return intersectForShadow(ray, max_t);
    }
}

double BSP::median(int d) const {
    std::list<double> L;
    for(unsigned int i = 0; i < objects.size(); i++) {
	    Object* obj = objects[i];
	    BoundingBox bbox = obj->boundingBoundingBox();
	    double c = (bbox.maximum()[d] + bbox.minimum()[d]) / 2.0;
	    L.push_back(c);
    }
    L.sort();
    unsigned int size = L.size();
    assert(size == objects.size());
    // Return L[size/2]
    unsigned int i = 0;
    for (std::list<double>::iterator h = L.begin(); h != L.end(); h++) {
	if (i++ > size/2) return *h;
    }
    exit(0);
}

Vector BSP::measureSplit(int dim, double val) const {
    Vector result = Vector(0,0,0);
    for(unsigned int i = 0; i < objects.size(); i++) {
	Object* obj = objects[i];
	BoundingBox bbox = obj->boundingBoundingBox();
	int cut_val = bbox.cutByPlane(dim, val);
	if (cut_val == -1) {
	    result[0]++;
	} else if (cut_val == 1) {
	    result[2]++;
	} else {
	    result[1]++;
	}
    }
    return result;
}

/*******************
 * Private stuff   *
 *******************/
bool BSP::intersectForShadow(const Ray& ray, const double min_t, const double max_t) const {
    if (max_t <= min_t) return false;

    if (objects.size() > 0) {
        Intersection tmp;
	for (unsigned int i = 0; i < objects.size(); i++) {
	    if ((!(objects[i]->getMaterial()->noShadow())) && objects[i]->intersect(ray) && objects[i]->getLastIntersection()->getT() <= max_t) {
		last_intersection = objects[i]->getLastIntersection();
		return true;
	    }
	}
	return false;
    } else {
        return intersectForShadow_recurse(ray,min_t,max_t);
    }
}

bool BSP::intersect(const Ray& ray, const double min_t, const double max_t) const {
    if (max_t <= min_t) return false;
    if (min_t > ray.lowest_t) return false;

    if (objects.size() > 0) {
	bool result = false;
	double smallest_t = HUGE_DOUBLE;
	for (unsigned int i = 0; i < objects.size(); i++) {
	    if (objects[i]->intersect(ray) && 
		   objects[i]->getLastIntersection()->getT() < smallest_t) {
		last_intersection = objects[i]->getLastIntersection();
		smallest_t = last_intersection->getT();
		result = true;
	    }
	}
	ray.lowest_t = min(ray.lowest_t,smallest_t);
	return result;
    } else {
        return intersect_recurse(ray,min_t,max_t);
    }
}

bool BSP::intersect_recurse(const Ray& ray, const double min_t, const double max_t) const {

    //Vector o = ray.getOrigin() + min_t * ray.getDirection();
    double rd_dim = ray.getDirection()[cutplane_dimension];
    double o_dim = ray.getOrigin()[cutplane_dimension] + min_t * rd_dim;
    
    if (o_dim < cutplane_value && rd_dim <= 0) {
	if (lower->intersect(ray,min_t,max_t)) {
	    last_intersection = lower->getLastIntersection();
	    return true;
	} else {
	    return false;
	}
    } else if (o_dim > cutplane_value && rd_dim >= 0) {
	if (higher->intersect(ray,min_t,max_t)) {
	    last_intersection = higher->getLastIntersection();
	    return true;
	} else {
	    return false;
	}
    } else {
	double intersect_t = (cutplane_value - ray.getOrigin()[cutplane_dimension]) * ray.getInverseDirection()[cutplane_dimension];
	if (intersect_t > max_t) { intersect_t = max_t; }
	if (intersect_t < min_t) { intersect_t = min_t; }

	bool intersection1;
	bool intersection2;

	if (o_dim < cutplane_value) {
	    // Ray is crossing the plane from a lower value
	    intersection1 = lower->intersect(ray,min_t,intersect_t);
	    intersection2 = higher->intersect(ray,intersect_t,max_t);
	} else {
	    // Ray is crossing the plane from a higher value
	    intersection2 = higher->intersect(ray,min_t,intersect_t);
	    intersection1 = lower->intersect(ray,intersect_t,max_t);
	}

	if (intersection1 && intersection2) {
	    if (higher->getLastIntersection()->getT() < lower->getLastIntersection()->getT()) {
		last_intersection = higher->getLastIntersection();
	    } else {
		last_intersection = lower->getLastIntersection();
	    }
	    return true;
	} else {
	    if (intersection1) {
		last_intersection = lower->getLastIntersection();
		return true;
	    } else if (intersection2) {
		last_intersection = higher->getLastIntersection();
		return true;
	    } else {
		return false;
	    }
	}
     }
}

bool BSP::intersectForShadow_recurse(const Ray& ray, const double min_t, const double max_t) const {
    if (max_t <= min_t) return false;

    double rd_dim = ray.getDirection()[cutplane_dimension];
    double o_dim = ray.getOrigin()[cutplane_dimension] + min_t * rd_dim;
    
    if (o_dim < cutplane_value && rd_dim <= 0) {
	if (lower->intersectForShadow(ray,min_t,max_t)) {
	    last_intersection = lower->getLastIntersection();
	    return true;
	} else {
	    return false;
	}
    } else if (o_dim > cutplane_value && rd_dim >= 0) {
	if (higher->intersectForShadow(ray,min_t,max_t)) {
	    last_intersection = higher->getLastIntersection();
	    return true;
	} else {
	    return false;
	}
    } else {
	
	double intersect_t = (cutplane_value - ray.getOrigin()[cutplane_dimension]) * ray.getInverseDirection()[cutplane_dimension];
	if (intersect_t > max_t) { intersect_t = max_t; }
	if (intersect_t < min_t) { intersect_t = min_t; }
	
	if (o_dim < cutplane_value) {
	    // Ray is crossing the plane from a lower value
	    if (lower->intersectForShadow(ray,min_t,intersect_t)) {
		last_intersection = lower->getLastIntersection();
		return true;
	    } else if (higher->intersectForShadow(ray,intersect_t,max_t)) {
		last_intersection = higher->getLastIntersection();
		return true;
	    } else {
		return false;
	    }
	} else {
	    // Ray is crossing the plane from a higher value
	    if (higher->intersectForShadow(ray,min_t,intersect_t)) {
		last_intersection = higher->getLastIntersection();
		return true;
	    } else if (lower->intersectForShadow(ray,intersect_t,max_t)) {
		last_intersection = lower->getLastIntersection();
		return true ;
	    } else {
		return false;
	    }
	}
    }
}

BoundingBox BSP::enclosure() const {
    assert(objects.size() > 0);
    BoundingBox result = objects[0]->boundingBoundingBox(); 
    for(unsigned int i = 1; i < objects.size(); i++) {
        result = BoundingBox::doUnion(result,objects[i]->boundingBoundingBox());
    }
    return result;
}

int BSP::largestDimension(const BoundingBox& box) {
    double x = box.maximum()[0] - box.minimum()[0];
    double y = box.maximum()[1] - box.minimum()[1];
    double z = box.maximum()[2] - box.minimum()[2];
    double max = MAX(x,MAX(y,z));
    if (IS_EQUAL(x,max)) {
	    return 0;
    } else if (IS_EQUAL(y,max)) {
	    return 1;
    } else if (IS_EQUAL(z,max)) {
	    return 2;
    } else {
	return -1;
       // Throw an exception
    }
}

void BSP::test() {
    BSP bsp;
    for(int x = -10; x <= 10; x++) {
       for(int y = -10; y <= 10; y++) {
           for(int z = -10; z <= 10; z++) {
	      Sphere* sx = new Sphere(Vector(x*20,y*20+50,z*20),10,NULL);
	      bsp.addObject(sx);
	   }
 	}
    }

    // Test enclosure()
    BoundingBox box = bsp.enclosure();
    assert(IS_EQUAL(box.minimum()[0],-210));
    assert(IS_EQUAL(box.minimum()[1],-160));
    assert(IS_EQUAL(box.minimum()[2],-210));
    assert(IS_EQUAL(box.maximum()[0],210));
    assert(IS_EQUAL(box.maximum()[1],260));
    assert(IS_EQUAL(box.maximum()[2],210));

    // Test largestDimension()
    bsp.addObject(new Sphere(Vector(0,-500,0),10,NULL));
    assert(largestDimension(bsp.enclosure()) == 1);
}


