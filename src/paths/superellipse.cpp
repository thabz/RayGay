
#include <cmath>
#include "paths/superellipse.h"

/**
 * Constructs a superellipse path
 *
 * @param center Center of the superellipse
 * @param radius Radius of the circle
 * @param normal Normal of the plane the circle should live in
 */
SuperEllipse::SuperEllipse(const Vector& center, double a, double b, double r, const Vector& normal) {
    this->c = center;
    this->r = r;
    this->a = a;
    this->b = b;
    n = normal;
    n.normalize();
    Vector y = Vector(0,1,0);
    Vector x = Vector(1,0,0);
    Vector A = n == y ? x : y; // TODO: This doesn't work... y-axis aligned normals fails
    m = Matrix::matrixOrient(n,Vector::xProduct(A,n));
    orient = m;
    m = m * Matrix::matrixTranslate(center);
}

Vector SuperEllipse::getPoint(double t) const {
    double rad = M_2PI * t;
    Vector result = Vector(cos(rad)*r,sin(rad)*r,0);
    return m * result;
}

Vector SuperEllipse::getTangent(double t) const {
    double rad = M_2PI * t;
    Vector result = Vector(-sin(rad),cos(rad),0);
    return orient * result;
}

void SuperEllipse::transform(const Matrix& m) {
    c = m * c;
    n = m.extractRotation() * n;
}
