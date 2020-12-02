
#include "objects/csg.h"
#include "aabox.h"
#include "exception.h"
#include <cassert>

CSGIntersection::CSGIntersection(Solid *left, Solid *right, const Material *mat)
    : Solid(mat) {
  this->left = left;
  this->right = right;
  this->max_intersections =
      left->maxIntersections() + right->maxIntersections();
}

CSGDifference::CSGDifference(Solid *left, Solid *right, const Material *mat)
    : Solid(mat) {
  this->left = left;
  this->right = right;
  this->max_intersections =
      left->maxIntersections() + right->maxIntersections();
}

SceneObject *CSGIntersection::clone() const {
  Solid *lhs = dynamic_cast<Solid *>(this->left->clone());
  Solid *rhs = dynamic_cast<Solid *>(this->right->clone());
  return new CSGIntersection(lhs, rhs, this->getMaterial());
}
SceneObject *CSGDifference::clone() const {
  Solid *lhs = dynamic_cast<Solid *>(this->left->clone());
  Solid *rhs = dynamic_cast<Solid *>(this->right->clone());
  return new CSGDifference(lhs, rhs, this->getMaterial());
}

AABox CSGDifference::getBoundingBox() const {
  AABox rb = right->getContainedBox();
  AABox lb = left->getBoundingBox();
  return AABox::doDifference(lb, rb);
}
AABox CSGIntersection::getBoundingBox() const {
  AABox rb = right->getBoundingBox();
  AABox lb = left->getBoundingBox();
  return AABox::doIntersection(rb, lb);
}

void CSGDifference::transform(const Matrix &m) {
  right->transform(m);
  left->transform(m);
}

void CSGIntersection::transform(const Matrix &m) {
  right->transform(m);
  left->transform(m);
}

bool CSGIntersection::inside(const Vector &p) const {
  return left->inside(p) && right->inside(p);
}

bool CSGDifference::inside(const Vector &p) const {
  return left->inside(p) && !right->inside(p);
}

double CSGIntersection::signedDistance(const Vector &p) const {
  return max(left->signedDistance(p), right->signedDistance(p));
}

double CSGDifference::signedDistance(const Vector &p) const {
  return max(left->signedDistance(p), -right->signedDistance(p));
}

uint32_t CSGIntersection::maxIntersections() const {
  if (max_intersections == 0)
    throw_exception("What!");
  return max_intersections;
}

uint32_t CSGDifference::maxIntersections() const {
  if (max_intersections == 0)
    throw_exception("What!");
  return max_intersections;
}

/**
 * Find all intersections.
 *
 * This is done by inverting the right side hit list and merging
 * the two lists by using the intersection-rule.
 *
 * \f[ R - L = R  \cap \neg L     \f]
 */
uint32_t CSGDifference::allIntersections(const Ray &ray,
                                         Intersection *result) const {
  Intersection *left_int =
      (Intersection *)::alloca(sizeof(Intersection) * left->maxIntersections());
  bool left_inside = false;
  uint32_t left_num = left->allIntersections(ray, left_int);
  if (left_num == 0)
    return 0;
  if (left_num > 0) {
    left_inside = !left_int[0].isEntering();
  }

  Intersection *right_int = (Intersection *)::alloca(sizeof(Intersection) *
                                                     right->maxIntersections());
  bool right_inside = false;
  uint32_t right_num = right->allIntersections(ray, right_int);
  if (right_num > 0) {
    right_inside = !right_int[0].isEntering();
  }

  // Invert all directions of right
  for (uint32_t i = 0; i < right_num; i++) {
    right_int[i].isEntering(!right_int[i].isEntering());
    right_int[i].flipNormal();
  }
  right_inside = !right_inside;
  if (right_num == 0 && !right_inside)
    return 0;

  // Merge intersections while preserving order
  uint32_t l = 0;
  uint32_t r = 0;
  uint32_t j = 0;
  while (l < left_num && r < right_num) {
    if (left_int[l].getT() < right_int[r].getT()) {
      Intersection i = left_int[l++];
      left_inside = i.isEntering();
      if (right_inside) {
        result[j++] = i;
      }
    } else {
      Intersection i = right_int[r++];
      right_inside = i.isEntering();
      if (left_inside) {
        result[j++] = i;
      }
    }
  }
  // Copy remaining if still inside other
  while (l < left_num && right_inside)
    result[j++] = left_int[l++];
  while (r < right_num && left_inside)
    result[j++] = right_int[r++];
  return j;
}

uint32_t CSGIntersection::allIntersections(const Ray &ray,
                                           Intersection *result) const {
  Intersection *left_int =
      (Intersection *)::alloca(sizeof(Intersection) * left->maxIntersections());
  uint32_t left_num = left->allIntersections(ray, left_int);
  bool left_inside = false;
  if (left_num > 0) {
    left_inside = !left_int[0].isEntering();
  } else {
    return 0;
  }

  Intersection *right_int = (Intersection *)::alloca(sizeof(Intersection) *
                                                     right->maxIntersections());
  uint32_t right_num = right->allIntersections(ray, right_int);
  bool right_inside = false;
  if (right_num > 0) {
    right_inside = !right_int[0].isEntering();
  } else {
    return 0;
  }

  // Merge intersections while preserving order
  uint32_t l = 0;
  uint32_t r = 0;
  uint32_t j = 0;
  while (l < left_num && r < right_num) {
    if (left_int[l].getT() < right_int[r].getT()) {
      Intersection i = left_int[l++];
      left_inside = i.isEntering();
      if (right_inside) {
        result[j++] = i;
      }
    } else {
      Intersection i = right_int[r++];
      right_inside = i.isEntering();
      if (left_inside) {
        result[j++] = i;
      }
    }
  }
  // Copy remaining if still inside other
  while (l < left_num && right_inside)
    result[j++] = left_int[l++];
  while (r < right_num && left_inside)
    result[j++] = right_int[r++];
  return j;
}

double CSGDifference::_fastIntersect(const Ray &ray) const {
  Intersection *all =
      (Intersection *)::alloca(sizeof(Intersection) * maxIntersections());
  uint32_t num = allIntersections(ray, all);
  return num == 0 ? -1 : all[0].getT();
}

double CSGIntersection::_fastIntersect(const Ray &ray) const {
  Intersection *all =
      (Intersection *)::alloca(sizeof(Intersection) * maxIntersections());
  uint32_t num = allIntersections(ray, all);
  return num == 0 ? -1 : all[0].getT();
}

void CSGDifference::_fullIntersect(const Ray &ray, const double t,
                                   Intersection &result) const {
  Intersection *all =
      (Intersection *)::alloca(sizeof(Intersection) * maxIntersections());
  uint32_t num = allIntersections(ray, all);
  if (num > 0) {
    result = all[0];
  } else {
    throw_exception("This shouldn't happen...");
  }
}

void CSGIntersection::_fullIntersect(const Ray &ray, const double t,
                                     Intersection &result) const {
  Intersection *all =
      (Intersection *)::alloca(sizeof(Intersection) * maxIntersections());
  uint32_t num = allIntersections(ray, all);
  if (num > 0) {
    result = all[0];
  } else {
    throw_exception("This shouldn't happen...");
  }
}
