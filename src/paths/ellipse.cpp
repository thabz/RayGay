
#include <cmath>
#include "paths/ellipse.h"

/**
 * Constructs an ellipse path
 *
 * @param center Center of the ellipse 
 * @param r1 X-radius of the ellipse
 * @param r2 Y-radius of the ellipse
 * @param normal Normal of the plane the circle should live in
 */
Ellipse::Ellipse(const Vector& center, double r1, double r2, const Vector& normal) {
    c = center;
    this->r1 = r1;
    this->r2 = r2;
    n = normal;
    n.normalize();
    Vector y = Vector(0,1,0);
    Vector x = Vector(1,0,0);
    Vector a = n == y ? x : y; // TODO: This doesn't work... y-axis aligned normals fails
    m = Matrix::matrixOrient(n,Vector::xProduct(a,n));
    orient = m;
    m = m * Matrix::matrixTranslate(center);
}

Vector Ellipse::getPoint(double t) const {
    double rad = M_2PI * t;
    Vector result = Vector(cos(rad)*r1, sin(rad)*r2, 0);
    return m * result;
}

Vector Ellipse::getTangent(double t) const {
    double rad = M_2PI * t;
    Vector result = Vector(-r1 * sin(rad), r2 * cos(rad),0);
    return orient * result;
}

void Ellipse::transform(const Matrix& m) {
    c = m * c;
    n = m.extractRotation() * n;
}
