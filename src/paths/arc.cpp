
#include "paths/arc.h"

/**
 * Constructor
 *
 * @param center center of the circle
 * @param radius radius of the circle 
 * @param normal normal to the plane the circle lies in
 * @param begin_degree begin degree of the arc in degrees
 * @param end_degree end degree of the arc in degrees
 */
Arc::Arc(const Vector& center, double radius, const Vector& normal, double begin_degree, double end_degree) {
    circle = new Circle(center,radius,normal);
    begin_t = DEG2RAD(begin_degree) / M_2PI;
    end_t = DEG2RAD(end_degree) / M_2PI;
}

/**
 * Constructor
 *
 * @param circle the circle whose perimiter the arc should lie in
 * @param begin_degree begin degree of the arc in degrees
 * @param end_degree end degree of the arc in degrees
 */
Arc::Arc(Circle* circle, double begin_degree, double end_degree) {
    this->circle = circle;
    begin_t = begin_degree / M_2PI;
    end_t = end_degree / M_2PI;
}

Vector Arc::getPoint(double t) const {
    return circle->getPoint(convert(t));
}

Vector Arc::getTangent(double t) const {
    return circle->getTangent(convert(t));
}

void Arc::transform(const Matrix& m) {
    circle->transform(m);
}

double Arc::convert(const double t) const {
    return (double(1)-t)*begin_t + t*end_t;
}

