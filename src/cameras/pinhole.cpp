
#include "cameras/pinhole.h"
#include "ray.h"
#include "math/functions.h"

Pinhole::Pinhole() : Camera () {
}

Ray Pinhole::getRay(const double x, const double y) {
    if (!initialized) 
	init();

    double du = -au + ((2.0 * au * x) / (width));
    double dv = -av + ((2.0 * av * y) / (height));
    Vector dir = basis * Vector(du,dv,-1);
    Vector pos = position;
    dir.normalize();

    if (dof_enabled) {
	// Jitter position and adjust direction

       Vector P = pos + dir * dof_length; // The point to aim at

       if (++dof_sample_count > dof_samples) {
	   dof_qmc->reset();
	   dof_sample_count = 0;
       }

       double* qmc = dof_qmc->getNext();
       Vector2 disc = Math::shirleyDisc(qmc[0],qmc[1]) * dof_aperture;
       Vector jitter_pos = up * disc[0] + right * disc[1];

       pos = pos + jitter_pos;
       dir = P - pos;
       dir.normalize();
    }
    
    return Ray(pos, dir, 1.0);
}

