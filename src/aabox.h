#ifndef BOUNDINGBOX_H
#define BOUNDINGBOX_H

#include "math/vector.h"
#include <iosfwd>
#include <vector>

class Intersection;
class Ray;
class Matrix;
class Vector2;

/// An axis-aligned box. Usually used for bounding stuff.

class AABox {

  friend std::ostream &operator<<(std::ostream &os, const AABox &b);

public:
  /// Constructor
  AABox();

  /// Constructor
  AABox(const Vector &corner1, const Vector &corner2);

  /// Constructor
  AABox(const std::vector<Vector> &swarm);

  /// Returns (tmin,tmax) of intersection
  Vector2 intersect(const Ray &ray) const;

  /// Simple check for intersection
  bool checkIntersect(const Ray &ray) const;

  /// The box' normal at a point. This vector is always axis-aligned.
  Vector normal(const Vector &p) const;

  /// Test whether a point belongs to the closure of this box.
  bool onEdge(const Vector &p) const;

  /// Tests whether a point is inside this box and not on the edge.
  bool inside(const Vector &p) const;
  //
  /// Tests whether a point is inside this box or on the edge.
  bool insideOrTouching(const Vector &p) const;

  /// Tests whether an array of points are inside this box and not on the edge.
  bool inside(const Vector *points, int num) const;

  /// Tests whether another boundingbox is inside this
  bool inside(const AABox &b) const;

  /// Tests whether this bbox is in back, front or is intersected by a
  /// axis-aligned plane.
  int cutByPlane(int cutplane_dimension, double cutplane_value) const;

  /// The corner with smallest x,y,z values
  const Vector minimum() const { return Vector(_c1[0], _c1[1], _c1[2]); };

  /// The corner with biggest x,y,z values
  const Vector maximum() const { return Vector(_c2[0], _c2[1], _c2[2]); };

  /// An x,y or z value from the corner with smallest x,y,z values
  double minimum(const uint32_t i) const { return _c1[i]; };

  /// An x,y or z value from the corner with biggest x,y,z values
  double maximum(const uint32_t i) const { return _c2[i]; };

  /// Returns the smallest box that contains b1 and b2.
  static AABox doUnion(const AABox &b1, const AABox &b2);

  /// Returns the smallest box that b1 and b2 have in common.
  static AABox doIntersection(const AABox &b1, const AABox &b2);

  /// Returns the smallest box that contains b1 - b2.
  static AABox doDifference(const AABox &b1, const AABox &b2);

  /// Returns the smallest box that inclose the points
  static AABox enclosure(Vector *points, int num);

  /// Returns a newly allocated length 8 array of the corners
  Vector *getCorners() const;

  /// Writes a length 8 array of the corners
  void getCorners(Vector *dest) const;

  /// Returns the area of this box' surface
  double area() const;

  /// Grow box 2*amount in all dimensions
  void grow(double amount);

  /// Grow box a percentage in all dimensions
  void growPercentage(double amount);

  /// Comparator
  bool operator==(const AABox &b) const;

  /// Says whether this bounding box intersects a sphere
  bool intersectSphere(const Vector &center, double squared_radius) const;

  /// The center point of the box
  Vector center() const;

  // Split this bbox into two by a axis-aligned plane
  bool split(AABox &left, AABox &right, const uint32_t dim,
             const double axis) const;

  // Lengths of the three sides
  Vector lengths() const;

  // Set minimum lengths
  void setMinimumLengths(double l);

private:
  double _c1[3]; ///< The point with smallest x,y,z values
  double _c2[3]; ///< The point with biggest x,y,z values
};

#endif
