
#include <cassert>
#include "path.h"
#include "vector.h"
#include "circle.h"
#include "matrix.h"
#include "math.h"
#include "constants.h"

/**
 * Constructs a circle path
 *
 * @param center Center of the circle
 * @param radius Radius of the circle
 * @param normal Normal of the plane the circle should live in
 */
Circle::Circle(const Vector& center, double radius, const Vector& normal) {
    c = center;
    r = radius;
    n = normal;
    n.normalize();
    m = Matrix::matrixOrient(n,Vector(0,1,0));
    m = m * Matrix::matrixTranslate(center);
}

Vector Circle::getPoint(double t) const {
    double rad = M_2PI * t;
    Vector result = Vector(cos(rad)*r,sin(rad)*r,0);
    return m * result;
}

Vector Circle::getTangent(double t) const {
    double rad = M_2PI * t;
    Vector result = Vector(-sin(rad),cos(rad),0);
    return m * result;
}

void Circle::test() {
    /* Test at origin */
    Circle c = Circle(Vector(0,0,0),10,Vector(0,0,1));
    assert(c.isClosed());
    assert(c.getPoint(0) == Vector(10,0,0));
    assert(c.getPoint(0.25) == Vector(0,10,0));
    assert(c.getPoint(0.5) == Vector(-10,0,0));

    /* Test translated */
    c = Circle(Vector(10,10,0),10,Vector(0,0,1));
    assert(c.getPoint(0) == Vector(20,10,0));
    assert(c.getPoint(0.25) == Vector(10,20,0));
    assert(c.getPoint(0.5) == Vector(0,10,0));

    /* Test direction */
    c = Circle(Vector(0,0,0),10,Vector(1,0,0));
    assert(c.getPoint(0) == Vector(0,0,-10));
    assert(c.getPoint(0.25) == Vector(0,10,0));
    assert(c.getPoint(0.5) == Vector(0,0,10));

    // TODO: Test at a direction not axis-aligned */
}
