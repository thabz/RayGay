
#include "wireframe.h"
#include "cylinder.h"
#include "mesh.h"
#include "objectgroup.h"
#include "paths/linesegment.h"
#include "sphere.h"
#include <vector>

using namespace std;

/**
 * The constructor
 *
 * @param mesh A mesh object
 * @param radius The radius of the generated cylinders and spheres
 * @param material The material
 */
Wireframe::Wireframe(Mesh *mesh, double radius, const Material *material) {
  vector<Linesegment> *edges = mesh->getEdges();
  uint32_t size = edges->size();
  for (uint32_t i = 0; i < size; i++) {
    Linesegment *line = &((*edges)[i]);
    addObject(
        new Cylinder(line->begin(), line->end(), radius, false, material));
  }
  delete edges;

  vector<Vector> *vertices = mesh->getVertices();
  for (uint32_t i = 0; i < vertices->size(); i++) {
    Vector c = (*vertices)[i];
    addObject(new Sphere(c, radius, material));
  }
  delete vertices;
}
