
#include "tetrahedron.h"
#include "mesh.h"
#include "vector.h"
#include <math.h>
#include <iostream>
#include <cassert>
#include <vector>
#include "paths/linesegment.h"

using namespace std;

Tetrahedron::Tetrahedron(const Vector a, const double r, Material mat) : Mesh(Mesh::MESH_FLAT,mat) {

    double sqrt_3 = 0.5773502692 * r; // Found on the internet
    Vector PPP = Vector( sqrt_3,  sqrt_3,  sqrt_3 ) + a; 
    Vector MMP = Vector(-sqrt_3, -sqrt_3,  sqrt_3 ) + a;
    Vector MPM = Vector(-sqrt_3,  sqrt_3, -sqrt_3 ) + a;
    Vector PMM = Vector( sqrt_3, -sqrt_3, -sqrt_3 ) + a;
    
    Mesh::addTriangle(MPM, MMP, PPP);
    Mesh::addTriangle(MMP, PMM, PPP);
    Mesh::addTriangle(PMM, MMP, MPM);
    Mesh::addTriangle(MPM, PPP, PMM);
}

Tetrahedron::~Tetrahedron() {
}

void Tetrahedron::test() {
    Material mat = Material(RGB(0,0,0),RGB(0,0,0));
    Tetrahedron t = Tetrahedron(Vector(0,0,0),100,mat);
    assert(t.getEdges()->size() == 6);
    assert(t.getVertices()->size() == 4);

    cout << "Tetrahedron::test() done." << endl;
}
