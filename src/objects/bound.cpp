
#include "bsp.h"
#include "boundingbox.h"
#include "objects/bound.h"
#include "objects/objectgroup.h"
#include "math/vector2.h"

Bound::Bound(ObjectGroup* group) : Object(NULL) {
    this->group = group;
}

void Bound::prepare() {
    tree = new KdTree();
    group->prepare();
    group->addSelf(tree);
}

void Bound::transform(const Matrix& m) {
    group->transform(m);
}

BoundingBox Bound::boundingBoundingBox() const {
    return tree->boundingBox();
}

Vector Bound::normal(const Intersection &i) const {
    return i.getObject()->normal(i);
}

Vector2 Bound::getUV(const Intersection& i) const {
    return i.getObject()->getUV(i);
}

SceneObject* Bound::clone() const {
    // Not allowed!
}

Intersection Bound::_intersect(const Ray& ray) const {
    if (tree->intersect(ray)) {
	return *(tree->getLastIntersection());
    } else {
	return Intersection();
    } 
}
