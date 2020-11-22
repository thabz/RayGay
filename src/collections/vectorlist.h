
#ifndef VECTORLIST_H
#define VECTORLIST_H

#include "math/vector.h"
#include <vector>

class Matrix;
class Matrix3;

/**
 * A less memoryexpensive replacement for stl::vector<Vector>
 */
class VectorList {

public:
  VectorList();
  uint32_t size() const;
  Vector get(const uint32_t i) const;
  void get(const uint32_t i, Vector &dest) const;
  void get(const uint32_t i, double dest[3]) const;
  void set(const uint32_t i, const Vector &v);
  uint32_t push_back(const Vector &w);
  void transform(const Matrix &M);
  void transform(const Matrix3 &M);
  void normalize();
  Vector operator[](const uint32_t i) const;
  void reserve(uint32_t num);

private:
  std::vector<double> v;
};

inline Vector VectorList::get(uint32_t i) const {
  uint32_t k = i * 3;
  return Vector(v[k], v[k + 1], v[k + 2]);
}

inline void VectorList::get(uint32_t i, Vector &dest) const {
  uint32_t k = i * 3;
  dest[0] = v[k + 0];
  dest[1] = v[k + 1];
  dest[2] = v[k + 2];
}

inline void VectorList::get(uint32_t i, double dest[3]) const {
  uint32_t k = i * 3;
  dest[0] = v[k + 0];
  dest[1] = v[k + 1];
  dest[2] = v[k + 2];
}

inline void VectorList::set(const uint32_t i, const Vector &w) {
  uint32_t k = i * 3;
  v[k + 0] = w[0];
  v[k + 1] = w[1];
  v[k + 2] = w[2];
}

inline Vector VectorList::operator[](const uint32_t i) const {
  uint32_t k = i * 3;
  return Vector(v[k], v[k + 1], v[k + 2]);
}

inline uint32_t VectorList::size() const { return v.size() / 3; }

#endif
