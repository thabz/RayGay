
#include "tessalation.h"

Tessalation::Tessalation(const Vector center, const double radius, const uint num, const Material* mat) : Mesh(Mesh::MESH_FLAT,mat) {

    max_splits = num;
    this->radius = radius;
    this->center = center;

    double sqrt_3 = 0.5773502692 * radius; // Found on the internet
    Vector PPP = Vector( sqrt_3,  sqrt_3,  sqrt_3 ) + center; 
    Vector MMP = Vector(-sqrt_3, -sqrt_3,  sqrt_3 ) + center;
    Vector MPM = Vector(-sqrt_3,  sqrt_3, -sqrt_3 ) + center;
    Vector PMM = Vector( sqrt_3, -sqrt_3, -sqrt_3 ) + center;
    
    split(MPM, MMP, PPP, 1);
    split(MMP, PMM, PPP, 1);
    split(PMM, MMP, MPM, 1);
    split(MPM, PPP, PMM, 1);
}

void Tessalation::split(const Vector& v1, const Vector& v2, const Vector& v3, uint depth) {
    if (depth > max_splits) {
	Mesh::addTriangle(v1,v2,v3);
    } else {
	Vector c = (v1 + v2 + v3) / double(3);
	c -= center;
	c.normalize();
	c *= radius;
	c += center;
	depth++;
	split(v1,v2,c,depth);
	split(v2,v3,c,depth);
	split(v3,v1,c,depth);
    }
}

