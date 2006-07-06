#include "objects/julia.h"

#define max_d 3

Julia::Julia(Quaternion c, uint32_t max_iter, uint32_t steps, double accuracy, double iso, Material* mat) : IsoSurface(steps,accuracy, iso, mat) {
    this->c = c;
    this->max_iter = max_iter;
}

// See:
// http://graphics.cs.uiuc.edu/svn/kcrane/web/project_qjulia_source.html
double Julia::evaluateFunction(const Vector& point) const 
{
    Quaternion z = Quaternion(0, point);

    uint32_t i = 0;
    double d = 0;
    while (i < max_iter && d < max_d) {
	z = z.sqr() + c;
    }

    if (i == max_iter || d >= max_d) {
	// Outside the Julia set
	return -1;
    } else {
	// Inside the Julia set
	return 1;
    }
}

AABox Julia::_getBoundingBox() const 
{
    return AABox(Vector(-max_d, -max_d, -max_d),
	         Vector( max_d,  max_d,  max_d));
}
