
#include "objectgroup.h"
#include "object.h"
#include "spacesubdivider.h"
#include "math/matrix.h"
#include <vector>

using namespace std;

/**
 * Transform the objects in this collection
 *
 * @param m The transformation Matrix
 */
void ObjectGroup::transform(const Matrix &m) {
    for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->transform(m);
    }
}

/**
 * Add the objects in this collection to a space
 * @param space The space to add to
 */
void ObjectGroup::addParts(SpaceSubdivider* space) {
    for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	space->addObject(*p);
    }
}

/**
 * Prepare the objects in this collection
 */
void ObjectGroup::prepare() {
    for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->prepare();
    }
}

/**
 * Add an subobject to this collection
 *
 * @param obj A subobject
 */
void ObjectGroup::addObject(object* obj) {
    objects.push_back(obj);
}
