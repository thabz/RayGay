
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
    if (fmod(point.x(),2*size) > size ) {
	return mat1;
    } else {
	return mat2;
    }
}

RGB Checker::getDiffuseColor(const Intersection& i) const {
    return materialAtPoint(i.getPoint())->getDiffuseColor(i);
}

