
#include "objects/blob.h"
#include "math/vector.h"
#include "math/matrix.h"

/**
 * @param iso the weight level defining the surface
 * @param steps the raymarching steps
 * @param accuracy the accuracy which is some low number
 * @param material material of the Blob
 */
Blob::Blob(double iso, unsigned int steps, double accuracy, Material* material) : IsoSurface(steps,accuracy,iso,material) {
    atoms_num = 0;
}

/**
 * Add an atom to the blob.
 * 
 * @param center center of the sphere
 * @param radius radius of the sphere
 * @param weight weight to use
 */
void Blob::addAtom(const Vector& center, double radius, double weight) {
    centers.push_back(center);
    radii.push_back(radius*radius);
    weights.push_back(weight);
    atoms_num++;

    Vector rrr = Vector(radius,radius,radius);
    BoundingBox box = BoundingBox(center-rrr,center+rrr);
    bbox = BoundingBox::doUnion(box,bbox);
}

/**
 * Using the metaball distribution model.
 */
/*
double Blob::evaluateFunction(const Vector& point) const {
    double sum = 0;
    for(int i = 0; i < atoms_num; i++) {
	double r = (centers[i] - point).length();
	if (r >= radii[i]) {
	    continue;
	} else if (r <= radii[i] / 3.0) {
	    double p = r / radii[i];
	    sum += weights[i] * (1.0 - 3.0 * p * p);
	} else {
	    double p = 1.0 - (r / radii[i]);
	    sum += 1.5 * weights[i] * p * p;
	}
    }
    return sum;
}
*/
double Blob::evaluateFunction(const Vector& point) const {
    double sum = 0;
    double one_ninth = 1.0 / 9.0;
    for(int i = 0; i < atoms_num; i++) {
	double rr = (centers[i] - point).norm();
	double RR = radii[i];
	if (rr >= RR) {
	    continue;
	} else {
	    double q = rr / RR;
	    double p = -22.0 + 17.0*q - 4.0*q*q;
	    p *= q * one_ninth;
	    sum += weights[i] * (1.0 + p);
	}
    }
    return sum;
}

SceneObject* Blob::clone() const {
    Blob* result = new Blob(*this);
    return result;
}

