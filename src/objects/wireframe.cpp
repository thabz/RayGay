
#include "wireframe.h"
#include "mesh.h"
#include "sphere.h"
#include "cylinder.h"
#include "paths/linesegment.h"
#include "materials/material.h"
#include "objectgroup.h"
#include <vector>

using namespace std;

/**
 * The constructor
 *
 * @param mesh A mesh object
 * @param radius The radius of the generated cylinders and spheres
 * @param material The material
 */
Wireframe::Wireframe(Mesh* mesh, double radius, const Material* material) {
    vector<Linesegment>* edges = mesh->getEdges();
    unsigned int size = edges->size();
    for(unsigned int i = 0; i < size; i++) {
	Linesegment* line = &((*edges)[i]);
	addObject(new Cylinder(line->begin(),line->end(),radius,material));
    }
    delete edges;

    vector<Vector>* vertices = mesh->getVertices();
    for(unsigned int i = 0; i < vertices->size(); i++) {
	Vector c = (*vertices)[i];
        addObject(new Sphere(c,radius,material));
    }
    delete vertices;
}

