
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

	// TODO: Put all objects into lower- or higher_objects
	for(int i = 0; i < objects.size(); i++) {
	    object* obj = objects[i];
	    bbox = obj->boundingBoundingBox();
	    int cut_val = bbox.cutByPlane(cutplane_dimension, cutplane_value);
	    if (cut_val == -1) {
		lower->addObject(obj);
	    } else if (cut_val == 1) {
		higher->addObject(obj);
	    } else {
                // TODO: Use an iterator and leave this obj in objects.
	    }
	}

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
