
#include "objects/blob.h"
#include "math/vector.h"
#include "math/matrix.h"

Blob::Blob(double surface_density, unsigned int steps, double accuracy, Material material) : IsoSurface(steps,accuracy,surface_density) {
    atoms_num = 0;
    this->material = material;
}

void Blob::addAtom(const Vector& center, double radius, double weight) {
    centers.push_back(center);
    radii.push_back(radius);
    weights.push_back(weight);
    atoms_num++;

    Vector rrr = Vector(radius,radius,radius);
    BoundingBox box = BoundingBox(center-rrr,center+rrr);
    bbox = BoundingBox::doUnion(box,bbox);
}

/**
 * Using the metaball distribution model.
 */
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

SceneObject* Blob::clone() const {
    Blob* result = new Blob(*this);
    return result;
}

void Blob::transform(const Matrix& m) {
    for(int i = 0; i < atoms_num; i++) {
	centers[i] = m * centers[i];
    }
}

const Material& Blob::getMaterial() const {
    return material;
}

