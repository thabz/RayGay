#include "objects/julia.h"

#define max_d 4
#define w_axis_offset 0.8

Julia::Julia(Quaternion c, uint32_t max_iter, uint32_t steps, double accuracy, Material* mat) : IsoSurface(steps,accuracy,0, mat) {
    this->c = c;
    this->max_iter = max_iter;
}

double Julia::evaluateFunction(const Vector& point) const 
{
    Quaternion z = Quaternion(point, w_axis_offset);

    uint32_t i = 0;
    double d = 0;
    while (i++ < max_iter && d < max_d*max_d) {
	z = z.sqr() + c;
	d = z.norm_squared();
    }

    if (d >= max_d*max_d) {
	// Outside the Julia set
	return -1;
    } else {
	// Inside the Julia set
	return 1;
    }
}

/**
 * Specialized function for finding normals. The Isosurface::normal()
 * method won't work, because the evaluateFunction() above only returns
 * descreet values.
 *
 * @see http://graphics.cs.uiuc.edu/svn/kcrane/web/project_qjulia_source.html
 */
Vector Julia::normal(const Vector& point) const
{
    Quaternion z = Quaternion(point, w_axis_offset);

    Quaternion x1_delta = z - Quaternion(accuracy, 0, 0, 0);
    Quaternion x2_delta = z + Quaternion(accuracy, 0, 0, 0);
    Quaternion y1_delta = z - Quaternion(0, accuracy, 0, 0);
    Quaternion y2_delta = z + Quaternion(0, accuracy, 0, 0);
    Quaternion z1_delta = z - Quaternion(0, 0, accuracy, 0);
    Quaternion z2_delta = z + Quaternion(0, 0, accuracy, 0);

    uint32_t i;
    for(i = 0; i < max_iter; i++) {
	x1_delta = x1_delta.sqr() + c;
	x2_delta = x2_delta.sqr() + c;
	y1_delta = y1_delta.sqr() + c;
	y2_delta = y2_delta.sqr() + c;
	z1_delta = z1_delta.sqr() + c;
	z2_delta = z2_delta.sqr() + c;
    }

    Vector normal = Vector(
	    x2_delta.norm() - x1_delta.norm(), 
	    y2_delta.norm() - y1_delta.norm(), 
	    z2_delta.norm() - z1_delta.norm());
    normal.normalize();
    return normal;
}


AABox Julia::_getBoundingBox() const 
{
    return AABox(Vector(-max_d, -max_d, -max_d),
	         Vector( max_d,  max_d,  max_d));
}

SceneObject* Julia::clone() const {
    return new Julia(*this);
}
