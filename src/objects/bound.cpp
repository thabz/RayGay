
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
}

void Bound::prepare() {
}

void Bound::transform(const Matrix& m) {
    group->transform(m);
}

AABox Bound::getBoundingBox() const {
    return tree->boundingBox();
}

SceneObject* Bound::clone() const {
    ObjectGroup* new_group = dynamic_cast<ObjectGroup*>(group->clone());
    return new Bound(new_group);
}

void Bound::fullIntersect(const Ray& ray, double t, Intersection& result) const {
    if (!tree->intersect(ray, result)) {
	result = Intersection();
    } 
}

double Bound::_fastIntersect(const Ray& ray) const {
    return tree->intersect(ray);         
}

void Bound::_fullIntersect(const Ray& ray, const double t, Intersection& result) const {
    assert(false); // Shouldn't be called as we override Object::fullIntersect() 
}
