
#include "objects/bound.h"
#include "space/kdtree.h"
#include "aabox.h"
#include "objects/objectgroup.h"
#include "math/vector2.h"
#include "materials/plastic.h"

Bound::Bound(ObjectGroup* group) : Object(NULL) {
    this->group = group;
    tree = new KdTree();
    group->prepare();
    group->addSelf(tree);
    tree->prepare();
    running = false;
}

void Bound::prepare() {
}

void Bound::transform(const Matrix& m) {
    group->transform(m);
}

AABox Bound::getBoundingBox() const {
    return tree->boundingBox();
}

Vector Bound::normal(const Intersection &i) const {
    // Not to be called
    return Vector(1,0,0);
}

Vector2 Bound::getUV(const Intersection& i) const {
    // Not to be called
    return Vector2(-1,-1);
}

SceneObject* Bound::clone() const {
    ObjectGroup* new_group = dynamic_cast<ObjectGroup*>(group->clone());
    return new Bound(new_group);
}

/*
const Material* Bound::getMaterial() const {
    if (last_intersection.isIntersected()) {
	return last_intersection.getObject()->getMaterial();
    } else {
	return NULL;
    }
}
*/

void Bound::fullIntersect(const Ray& ray, double t, Intersection& result) const {
    if (!tree->intersect(ray, result)) {
	result = Intersection();
    } 
}

/**
 * TODO: Inefficient method 
 */
double Bound::fastIntersect(const Ray& ray) const {
    Intersection result;
    if (tree->intersect(ray, result)) {
	return result.getT();
    } else {
	return -1;
    }
}

double Bound::_fastIntersect(const Ray& ray) const {
    return fastIntersect(ray);
}

void Bound::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
    return fullIntersect(ray,t, result);
}
