
#include "tetrahedron.h"
#include "mesh.h"
#include "vector.h"
#include "material.h"
#include <math.h>
#include <iostream>
#include <cassert>
#include <vector>
#include "linesegment.h"

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
    t.prepare();
    vector<Linesegment>* edges = t.getEdges();
    assert(edges->size() == 6);
    delete edges;

    vector<Vector>* vertices = t.getVertices();
    assert(vertices->size() == 4);
    delete vertices;

    cout << "Tetrahedron::test() done." << endl;
}
