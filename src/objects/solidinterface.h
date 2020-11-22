
#ifndef OBJECTS_SOLID_INTERFACE_H
#define OBJECTS_SOLID_INTERFACE_H

#include "objects/object.h"
#include <vector>

/**
 * Objects that can be used as part of CSG objects must
 * implement this interface.
 */
class SolidInterface {

public:
  /**
   * Find all intersections with ray.
   *
   * The intersections are added to the result vector. The intersections
   * are sorted by distance along ray with nearest first.
   *
   * @param ray the ray to intersect with.
   * @param result the intersections are added here.
   */
  virtual uint32_t allIntersections(const Ray &ray,
                                    Intersection *result) const = 0;

  /**
   * Returns the maximum number of intersections allIntersections(...) can
   * return.
   */
  virtual uint32_t maxIntersections() const = 0;

  /**
   * Returns largest AABox inscribed in this object.
   */
  virtual AABox getContainedBox() const = 0;

  /**
   * Says whether a point is inside the object
   */
  virtual bool inside(const Vector &p) const = 0;
};

#endif
