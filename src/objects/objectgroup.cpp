
#include "objectgroup.h"
#include "object.h"
#include "space/kdtree.h"
#include "math/matrix.h"
#include <vector>

using namespace std;

ObjectGroup::ObjectGroup() {
}

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
void ObjectGroup::addSelf(KdTree* space) {
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
    uint32_t num = this->objects.size();
    for (uint32_t i = 0; i < num; i++) {
	result->addObject(this->objects[i]->clone());
    }
    return result;
}


std::vector<SceneObject*> ObjectGroup::getObjects() const {
    vector<SceneObject*> result = objects;
    return objects;
}
