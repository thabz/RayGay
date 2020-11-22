
#include "box.h"

#include <cassert>
#include <iostream>

#include "image/rgb.h"
#include "intersection.h"
#include "math/matrix.h"
#include "mesh.h"
#include "ray.h"

Box::Box(const Vector a, const Vector b, const Material *mat)
    : Mesh(Mesh::MESH_FLAT, mat) {
  prepareBox(a, b);
}

Box::Box(const Vector c, double width, double height, double depth, Material *m)
    : Mesh(Mesh::MESH_FLAT, m) {
  Vector extend = Vector(width, height, depth) / 2;
  prepareBox(c - extend, c + extend);
}

void Box::prepareBox(const Vector &a, const Vector &b) {
  // Back left bottom
  Vector aa = Vector(min(a[0], b[0]), min(a[1], b[1]), min(a[2], b[2]));
  // Front right top
  Vector bb = Vector(max(a[0], b[0]), max(a[1], b[1]), max(a[2], b[2]));

  uint32_t c2 = addVertex(aa);
  uint32_t c7 = addVertex(bb);

  // Back right bottom
  uint32_t c1 = addVertex(Vector(bb[0], aa[1], aa[2]));
  // Back left top
  uint32_t c4 = addVertex(Vector(aa[0], bb[1], aa[2]));
  // Back right top
  uint32_t c3 = addVertex(Vector(bb[0], bb[1], aa[2]));
  // Front right bottom
  uint32_t c5 = addVertex(Vector(bb[0], aa[1], bb[2]));
  // Front left bottom
  uint32_t c6 = addVertex(Vector(aa[0], aa[1], bb[2]));
  // Front left top
  uint32_t c8 = addVertex(Vector(aa[0], bb[1], bb[2]));

  // Bottom left
  uint32_t uv00 = addUV(Vector2(0.0, 1.0));
  // Top left
  uint32_t uv01 = addUV(Vector2(0.0, 0.0));
  // Bottom right
  uint32_t uv10 = addUV(Vector2(1.0, 1.0));
  // Top right
  uint32_t uv11 = addUV(Vector2(1.0, 0.0));

  // Back face
  Mesh::addTriangle(c1, c2, c3, uv00, uv10, uv01);
  Mesh::addTriangle(c2, c4, c3, uv10, uv11, uv01);
  // Left face
  Mesh::addTriangle(c2, c6, c4, uv00, uv10, uv01);
  Mesh::addTriangle(c6, c8, c4, uv10, uv11, uv01);
  // Bottom face
  Mesh::addTriangle(c5, c6, c2, uv11, uv01, uv00);
  Mesh::addTriangle(c1, c5, c2, uv10, uv11, uv00);
  // Front face
  Mesh::addTriangle(c6, c5, c7, uv00, uv10, uv11);
  Mesh::addTriangle(c6, c7, c8, uv00, uv11, uv01);
  // Right face
  Mesh::addTriangle(c5, c1, c7, uv00, uv10, uv01);
  Mesh::addTriangle(c1, c3, c7, uv10, uv11, uv01);
  // Top face
  Mesh::addTriangle(c3, c4, c7, uv11, uv01, uv10);
  Mesh::addTriangle(c4, c8, c7, uv01, uv00, uv10);
  Mesh::prepare();
}

Box::~Box() {
  // Do we leak stuff inside Mesh here?
}
