
#include "objectgroup.h"
#include "object.h"
#include "space/spacesubdivider.h"
#include "math/matrix.h"
#include <vector>

using namespace std;

/**
 * Transform the objects in this collection
 *
 * @param m The transformation Matrix
 */
void ObjectGroup::transform(const Matrix &m) {
    for (vector<SceneObject*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->transform(m);
    }
}

/**
 * Add the objects in this collection to a space
 * @param space The space to add to
 */
void ObjectGroup::addSelf(SpaceSubdivider* space) {
    for (vector<SceneObject*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->addSelf(space);
    }
}

/**
 * Prepare the objects in this collection
 */
void ObjectGroup::prepare() {
    for (vector<SceneObject*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->prepare();
    }
}

/**
 * Add an subobject to this collection
 *
 * @param obj A subobject
 */
void ObjectGroup::addObject(SceneObject* obj) {
    objects.push_back(obj);
}

SceneObject* ObjectGroup::clone() const {
    ObjectGroup* result = new ObjectGroup();
    unsigned int num = this->objects.size();
    for (unsigned int i = 0; i < num; i++) {
	result->addObject(this->objects[i]->clone());
    }
    return result;
}

