
#include "box.h"

#include <iostream>
#include <cassert>

#include "vector.h"
#include "ray.h"
#include "intersection.h"
#include "matrix.h"
#include "rgb.h"
#include "material.h"
#include "constants.h"
#include "mesh.h"

Box::Box(const Vector c1, const Vector c2, Material mat) {
    Vector _c1 = Vector(min(c1[0],c2[0]),min(c1[1],c2[1]),min(c1[2],c2[2]));
    Vector _c2 = Vector(max(c1[0],c2[0]),max(c1[1],c2[1]),max(c1[2],c2[2]));
    mesh = new Mesh(Mesh::MESH_FLAT,mat);
}

Box::~Box() {
    delete mesh;
}

