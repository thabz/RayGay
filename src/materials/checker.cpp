
#include "materials/checker.h"
#include "intersection.h"
#include <iostream>

using namespace std;

Checker::Checker(Material* mat1, Material* mat2, double size) : Material() {
    this->mat1 = mat1;
    this->mat2 = mat2;
    this->size = size;
}

Material* Checker::materialAtPoint(const Vector& point) const {
    Vector p = point / size;
    unsigned int x = (unsigned int) p.x() % 2;
    unsigned int y = (unsigned int) p.y() % 2;
    unsigned int z = (unsigned int) p.z() % 2;
    
    if (IS_NEGATIVE(p.z())) z ^= 1;
    if (IS_NEGATIVE(p.x())) x ^= 1;
    if (IS_NEGATIVE(p.y())) y ^= 1;

    if ((x+y+z) & 1 == 1) {
	return mat1;
    } else {
	return mat2;
    }
}

RGB Checker::getDiffuseColor(const Intersection& i) const {
    return materialAtPoint(i.getPoint())->getDiffuseColor(i);
}

