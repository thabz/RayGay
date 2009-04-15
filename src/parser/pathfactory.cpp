
#include <iostream>
#include <string>
#include "parser/pathfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "paths/circle.h"
#include "paths/ellipse.h"
#include "paths/linesegment.h"
#include "paths/bezierspline.h"
#include "paths/catmullromspline.h"
#include "paths/spiral.h"
#include "math/vector.h"

using namespace std;

/**
 * Circle-factory.
 * 
 * Usage:
 *  (make-circle center radius normal) -> path
 */
SchemeObject* PathFactory::make_circle(Scheme* scheme, SchemeObject* s_center, SchemeObject* s_radius, SchemeObject* s_normal) {
    const wchar_t* proc = L"make-circle";
    Vector center = scm2vector(s_center, proc, 1);
    double radius = safe_scm2double(s_radius, 2, proc);
    Vector normal = scm2vector(s_normal, proc, 3);
    return path2scm(new Circle(center,radius,normal));
}

/**
 * Ellipse-factory.
 * 
 * Usage:
 *  (make-ellipse center radius1 radius2 normal) -> path
 */
SchemeObject* PathFactory::make_ellipse(Scheme* scheme, SchemeObject* s_center, SchemeObject* s_radius1, SchemeObject* s_radius2, SchemeObject* s_normal) {
    const wchar_t* proc = L"make-ellipse";
    Vector center = scm2vector(s_center, proc, 1);
    Vector normal = scm2vector(s_normal, proc, 2);
    double radius1 = safe_scm2double(s_radius1,3,proc);
    double radius2 = safe_scm2double(s_radius2,4,proc);
    return path2scm(new Ellipse(center, radius1, radius2, normal));
}

/**
 * Linesegment-factory.
 * 
 * Usage:
 *  (make-linesegment from to) -> path
 */
SchemeObject* PathFactory::make_linesegment(Scheme* scheme, SchemeObject* s_from, SchemeObject* s_to) {
    const wchar_t* proc = L"make-linesegment";
    Vector from = scm2vector(s_from, proc, 1);
    Vector to = scm2vector(s_to, proc, 2);
    return path2scm(new Linesegment(from, to));
}

/**
 * Spiral-factory.
 *
 * Usage:
 * (make-spiral path radius windings offset) -> path
 */
SchemeObject* PathFactory::make_spiral(Scheme* scheme, SchemeObject* s_path, SchemeObject* s_radius, SchemeObject* s_windings, SchemeObject* s_offset) {
    const wchar_t* proc = L"make-spiral";
    Path* path = scm2path(s_path, proc, 1);
    double radius = safe_scm2double(s_radius,2,proc);
    double windings = safe_scm2double(s_windings,3,proc);
    double offset = safe_scm2double(s_offset,4,proc);
    return path2scm(new Spiral(path,radius,windings,offset));
}

SchemeObject* PathFactory::make_bezierspline(Scheme* scheme, SchemeObject* s_vector_vector) {
    vector<Vector> vectors = scm2vectorlist(s_vector_vector, L"make-bezierspline",1);
    return path2scm(new BezierSpline(vectors));
}

SchemeObject* PathFactory::make_catmullrom_spline(Scheme* scheme, SchemeObject* s_vector_vector) {
    vector<Vector> vectors = scm2vectorlist(s_vector_vector, L"make-catmullrom-spline",1);
    return path2scm(new CatmullRomSpline(vectors));
}


/**
 * Get point on path.
 *
 * Usage:
 * (point-on-path path t) -> vector
 */
SchemeObject* PathFactory::point_on_path(Scheme* scheme, SchemeObject* s_path, SchemeObject* s_t) {
    Path* path = scm2path(s_path, L"point-on-path", 1);
    double t = safe_scm2double(s_t, 2, L"point-on-path");
    Vector v = path->getPoint(t);
    return vector2scm(v);
}

/**
 * Get tangent to path.
 *
 * Usage:
 * (tangent-on-path path t) -> vector
 */
SchemeObject* PathFactory::tangent_to_path(Scheme* scheme, SchemeObject* s_path, SchemeObject* s_t) {
    Path* path = scm2path(s_path, L"tangent-to-path", 1);
    double t = safe_scm2double(s_t, 2, L"tangent-to-path");
    Vector v = path->getTangent(t);
    return vector2scm(v);
}

void PathFactory::register_procs(Scheme* scheme) {
    scheme->assign(L"make-circle",3,0,0,(SchemeObject* (*)()) PathFactory::make_circle);
    scheme->assign(L"make-ellipse",4,0,0,(SchemeObject* (*)()) PathFactory::make_ellipse);
    scheme->assign(L"make-linesegment",2,0,0,(SchemeObject* (*)()) PathFactory::make_linesegment);
    scheme->assign(L"make-spiral",4,0,0,(SchemeObject* (*)()) PathFactory::make_spiral);
    scheme->assign(L"make-bezierspline",1,0,0,(SchemeObject* (*)()) PathFactory::make_bezierspline);
    scheme->assign(L"make-catmullrom-spline",1,0,0,(SchemeObject* (*)()) PathFactory::make_catmullrom_spline);
    scheme->assign(L"point-on-path",2,0,0,(SchemeObject* (*)()) PathFactory::point_on_path);
    scheme->assign(L"tangent-to-path",2,0,0,(SchemeObject* (*)()) PathFactory::point_on_path);
}
