
#include "tetrahedron.h"
#include "mesh.h"
#include "vector.h"
#include "material.h"
#include <math.h>

Tetrahedron::Tetrahedron(const Vector a, const double r, Material mat) : Mesh(Mesh::MESH_FLAT,mat) {

    double sqrt_3 = 0.5773502692 * r;
    Vector PPP = Vector( sqrt_3,  sqrt_3,  sqrt_3 ) + a; /* +X, +Y, +Z */
    Vector MMP = Vector(-sqrt_3, -sqrt_3,  sqrt_3 ) + a ; /* -X, -Y, +Z */
    Vector MPM = Vector(-sqrt_3,  sqrt_3, -sqrt_3 ) + a; /* -X, +Y, -Z */
    Vector PMM = Vector( sqrt_3, -sqrt_3, -sqrt_3 ) + a; /* +X, -Y, -Z */
    

    /*
    Mesh::addTriangle(PPP, MMP, MPM);
    Mesh::addTriangle(PPP, PMM, MMP);
    Mesh::addTriangle(MPM, MMP, PMM);
    Mesh::addTriangle(PMM, PPP, MPM);
    */
    Mesh::addTriangle(MPM, MMP, PPP);
    Mesh::addTriangle(MMP, PMM, PPP);
    Mesh::addTriangle(PMM, MMP, MPM);
    Mesh::addTriangle(MPM, PPP, PMM);
}

Tetrahedron::~Tetrahedron() {
    // Is ~Mesh() called automatically?
}
