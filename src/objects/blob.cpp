
#include "objects/blob.h"
#include "math/vector.h"
#include "math/matrix.h"

Blob::Blob(double surface_density, unsigned int steps, double accuracy, Material material) : IsoSurface(steps,accuracy,surface_density) {
    atoms_num = 0;
    this->material = material;
}

void Blob::addAtom(const Vector& center, double r, double b) {
    centers.push_back(center);
    as.push_back(r);
    bs.push_back(b);
    atoms_num++;

    Vector rrr = Vector(r,r,r);
    BoundingBox box = BoundingBox(center-rrr,center+rrr);
    bbox = BoundingBox::doUnion(box,bbox);
}

/**
 * Using Blinn's model where the density is
 *
 * \f[ D(P) = b e^{-ar^2} \f]
 * 
 * Where \f$ r \f$ is the distance from point \f$ P \f$ to the centre of the atom.
 * The blobbiness can be controlled by the \f$ a \f$ and \f$ b \f$ parameters
 */
double Blob::evaluateFunction(const Vector& point) const {
    double sum = 0;
    for(int i = 0; i < atoms_num; i++) {
	double r2 = (centers[i] - point).norm();
	sum += bs[i] * exp(-as[i]*r2);
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

