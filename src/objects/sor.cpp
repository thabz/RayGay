#include "sor.h"
#include "paths/circle.h"
#include <cassert>

SurfaceOfRevolution::SurfaceOfRevolution(const vector<Vector2> &_points,
                                         uint32_t segments, const Material *m)
    : Mesh(Mesh::MESH_FLAT, m) {

  int points_num = _points.size();
  assert(points_num >= 2);

  Vector *bp = new Vector[segments];
  Vector *ep = new Vector[segments];

  vector<Vector2> points = _points;

  /*
  // Close ends if necessay
  if (IS_NZERO(points[0][0])) {
      points.insert(points.begin(),Vector2(0,points[0][1]));
  }
  if (IS_NZERO(points[points_num-1][0])) {
      points.push_back(Vector2(0,points[points_num-1][1]));
  }
  */

  // Convert to triangle mesh
  for (int y = 0; y < points_num - 1; y++) {
    Circle b =
        Circle(Vector(0, points[y][1], 0), points[y][0], Vector(0, 1, 0));
    Circle e = Circle(Vector(0, points[y + 1][1], 0), points[y + 1][0],
                      Vector(0, 1, 0));
    b.getPoints(segments, bp);
    e.getPoints(segments, ep);

    for (uint32_t i = 0; i < segments; i++) {
      uint32_t j = (i + 1) % segments;
      addTriangle(addVertex(ep[j]), addVertex(bp[j]), addVertex(bp[i]));
      addTriangle(addVertex(ep[i]), addVertex(ep[j]), addVertex(bp[i]));
    }
  }

  delete[] bp;
  delete[] ep;
}
