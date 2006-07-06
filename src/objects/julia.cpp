#include "objects/julia.h"

#define max_d 4

Julia::Julia(Quaternion c, uint32_t max_iter, uint32_t steps, double accuracy, Material* mat) : IsoSurface(steps,accuracy,0, mat) {
    this->c = c;
    this->max_iter = max_iter;
}

// See:
// http://graphics.cs.uiuc.edu/svn/kcrane/web/project_qjulia_source.html
double Julia::evaluateFunction(const Vector& point) const 
{
    Quaternion z = Quaternion(point, 0);

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

Vector Julia::normal(const Vector& point) const
{
    Quaternion z = Quaternion(point, 0);

    Quaternion gx1 = z - Quaternion(accuracy, 0, 0, 0);
    Quaternion gx2 = z + Quaternion(accuracy, 0, 0, 0);
    Quaternion gy1 = z - Quaternion(0, accuracy, 0, 0);
    Quaternion gy2 = z + Quaternion(0, accuracy, 0, 0);
    Quaternion gz1 = z - Quaternion(0, 0, accuracy, 0);
    Quaternion gz2 = z + Quaternion(0, 0, accuracy, 0);

    uint32_t i;
    for(i = 0; i < max_iter; i++) {
	gx1 = gx1.sqr() + c;
	gx2 = gx2.sqr() + c;
	gy1 = gy1.sqr() + c;
	gy2 = gy2.sqr() + c;
	gz1 = gz1.sqr() + c;
	gz2 = gz2.sqr() + c;
    }

    double gradX = gx2.norm() - gx1.norm();
    double gradY = gy2.norm() - gy1.norm();
    double gradZ = gz2.norm() - gz1.norm();

    Vector normal = Vector(gradX, gradY, gradZ);
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
