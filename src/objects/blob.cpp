
Blob::Blob(double surface_density, unsigned int steps, double accuracy) : Volume(steps,accuracy) {
    this->surface_density = surface_density;
    atoms_num = 0;
}

void Blob::addAtom(const Vector& center, double a, double b) {
    centers.push_back(center);
    as.push_back(a);
    bs.push_back(b);
    atoms_num++;
}

bool Blob::inside(const Vector& point) const {
    return getDensity(point) >= surface_density;
}

/**
 * Using Blinn's model where the density is
 * \f[ D(P) = b e^{-ar^2} \f]
 * 
 * Where \f$ r \f$ is the distance from point \f$ P \f$ to the centre of the atom.
 * The blobbiness can be controlled by the \f$ a \f$ and \f$ b \f$ parameters
 */
double Blob::getDensity(const Vector& point) const {
    double sum = 0;
    for(int i = 0; i < atoms_num; i++) {
	double r2 = (centers[i] - point).norm();
	sum += bs[i] * exp(-as[i]*r2);
    }
    return sum;
}

