
#include "catmullromspline.h"
#include "math/vector.h"
#include "math/matrix.h"
#include <cassert>

/**
 * Constructor. 
 *
 * The constructor takes an array of \f$ N+1 \f$ control points. Note that
 * the curve only passes through points \f$ P_1, P_2, \ldots, P_{N-1} \f$.
 *
 * The endpoints \f$ P_0 \f$ and \f$ P_N \f$ only specify the
 * initial and terminating tangents.
 *
 * @param points the control points \f$ P_0, P_1, \ldots, P_N \f$
 */
CatmullRomSpline::CatmullRomSpline(const std::vector<Vector>& points) {
    P = points;
    points_num = P.size();
    assert(points_num > 3);
}

/**
 * Constructor. 
 *
 * The constructor takes an array of \f$ N+1 \f$ control points. Note that
 * the curve only passes through points \f$ P_1, P_2, \ldots, P_{N-1} \f$.
 *
 * The endpoints \f$ P_0 \f$ and \f$ P_N \f$ only specify the
 * initial and terminating tangents.
 *
 * @param points the control points \f$ P_0, P_1, \ldots, P_N \f$
 */
CatmullRomSpline::CatmullRomSpline(Vector* points, unsigned int num) {
    assert(num > 3);
    points_num = num;

    for(unsigned int i = 0; i < num; i++) {
	P.push_back(points[i]);
    }
}

CatmullRomSpline::~CatmullRomSpline() {
    P.clear();
}

Vector CatmullRomSpline::getPoint(double t) const {
    unsigned int i = segmentBegin(t);
    t = adjustT(t);
    double tt = t*t;
    double ttt = tt*t;
    return 0.5* ((-ttt + 2*tt - t)   * P[i-1] +
	         (3*ttt - 5*tt + 2)  * P[i]   +
   	         (-3*ttt + 4*tt + t) * P[i+1] +
	         (ttt - tt)          * P[i+2]);
}

Vector CatmullRomSpline::getTangent(double t) const {
    unsigned int i = segmentBegin(t);
    t = adjustT(t);
    double tt = t*t;
    Vector res;
    res = 0.5* ((-3*tt + 4*t - 1)   * P[i-1] +
	         (9*tt - 10*t)       * P[i]   +
   	         (-9*tt + 8*t + 1)   * P[i+1] +
	         (3*tt - 2*t)        * P[i+2]);
    res.normalize();
    return res;
}

void CatmullRomSpline::transform(const Matrix& m) {
    for(unsigned int i = 0; i < points_num; i++) {
	P[i] = m * P[i];
    }
}

unsigned int CatmullRomSpline::segmentBegin(const double t) const {
    // TODO: Test!
    unsigned int segments = points_num - 3;
    if (t == 1) return segments + 1;
    if (t == 0) return 1;
    return (unsigned int)(floor(t * segments));
}

double CatmullRomSpline::adjustT(const double t) const {
    int segments = points_num - 3;
    return fmod(t*segments, double(segments));
}
