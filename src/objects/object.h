
#ifndef OBJECT_H
#define OBJECT_H

#include <string>

#include "intersection.h"
#include "sceneobject.h"

class Matrix;
class Ray;
class Vector;
class Vector2;
class AABox;
class Material;
class KdTree;

/// The abstract superclass of all objects in the scene that can be rendered.

class Object : public SceneObject {
public:
  /// Returns a full intersection at t distance from ray's origin
  virtual void fullIntersect(const Ray &ray, const double t,
                             Intersection &result) const;
  double fastIntersect(const Ray &ray) const;

  /// Transform this object
  virtual void transform(const Matrix &m) = 0;

  /// Returns the material of this object
  virtual const Material *getMaterial() const { return material; };

  /// The smallest box containing this object
  virtual AABox getBoundingBox() const = 0;

  /**
   * Refine an intersection. This is used when pruning the Kd-Tree.
   * Many objects have empty space in their bounding boxes. If an
   * voxel only intersects empty space in an objects bounding box
   * the objects can safely be removed from said voxel during pruning.
   *
   * This methods allows for a more refined test of whether an object
   * lies in a voxel's bounding box.
   */
  virtual int intersects(const AABox &voxel_bbox, const AABox &obj_bbox) const;

  /// Prepares the object before rendering
  virtual void prepare();

  /// Add this object to the kd-tree
  void addSelf(KdTree *space);

  /// Returns the surface area of object
  virtual double area() const;

  /// Whether this object can selfshadow
  virtual bool canSelfshadow() const;

protected:
  /// Constructor
  Object(const Material *material);

  /// Internal intersect method that subclasses must implement
  virtual void _fullIntersect(const Ray &ray, const double t,
                              Intersection &result) const = 0;
  /// Internal fast intersect method that subclasses must implement
  virtual double _fastIntersect(const Ray &ray) const = 0;

private:
  /// The material of this object
  const Material *material;
};

inline void Object::fullIntersect(const Ray &ray, const double t,
                                  Intersection &result) const {
  _fullIntersect(ray, t, result);
  result.setObject(this);
}

/**
 *  Finds the smallest distance along a ray where this object is intersected by
 * the ray.
 *
 *  @param ray the Ray to intersect with
 *  @return positive distance along ray if and only if an intersection occured;
 * otherwise -1 is returned.
 *
 * I used to do mailboxing here, but with the new Kd-Tree traversal
 * algorithm mailboxing didn't add anything.
 */
inline double Object::fastIntersect(const Ray &ray) const {
  return _fastIntersect(ray);
}

#endif
