
#include "bsp.h"
#include "object.h"
#include "ray.h"
#include "intersection.h"

BSP::BSP() {
    cutplane_dimension = 0;
    cutplane_value = 0;
}

void BSP::addObject(object* obj) {
    objects.push_back(obj);
}

void BSP::prepare() {
    if (objects.size() > BSP_MAX) {
	// TODO: Find the cutplane_dimension and cutplane_value

	lower = new BSP();
	higher = new BSP();
	// TODO: Put all objects into lower- or higher_objects
	
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

