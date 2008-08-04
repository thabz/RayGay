
#include "tetrahedron.h"
#include "mesh.h"

Tetrahedron::Tetrahedron(const Vector a, const double r, const Material* mat) : Mesh(Mesh::MESH_FLAT,mat) {

    double sqrt_3 = 0.577350269189626 * r; // Found on the internet
    uint32_t PPP = addVertex(Vector( sqrt_3,  sqrt_3,  sqrt_3 ) + a); 
    uint32_t MMP = addVertex(Vector(-sqrt_3, -sqrt_3,  sqrt_3 ) + a);
    uint32_t MPM = addVertex(Vector(-sqrt_3,  sqrt_3, -sqrt_3 ) + a);
    uint32_t PMM = addVertex(Vector( sqrt_3, -sqrt_3, -sqrt_3 ) + a);
    
    Mesh::addTriangle(MPM, MMP, PPP);
    Mesh::addTriangle(MMP, PMM, PPP);
    Mesh::addTriangle(PMM, MMP, MPM);
    Mesh::addTriangle(MPM, PPP, PMM);
}

