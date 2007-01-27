
#include <iostream>

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
SCM PathFactory::make_circle(SCM s_center, SCM s_radius, SCM s_normal) {
    char* proc = "make-circle";
    Vector center = scm2vector(s_center, proc, 1);
    double radius = scm_num2double(s_radius, 2, proc);
    Vector normal = scm2vector(s_normal, proc, 3);
    return path2scm(new Circle(center,radius,normal));
}

/**
 * Ellipse-factory.
 * 
 * Usage:
 *  (make-ellipse center radius1 radius2 normal) -> path
 */
SCM PathFactory::make_ellipse(SCM s_center, SCM s_radius1, SCM s_radius2, SCM s_normal) {
    char* proc = "make-ellipse";
    Vector center = scm2vector(s_center, proc, 1);
    Vector normal = scm2vector(s_normal, proc, 2);
    double radius1 = scm_num2double(s_radius1,3,proc);
    double radius2 = scm_num2double(s_radius2,4,proc);
    return path2scm(new Ellipse(center, radius1, radius2, normal));
}

/**
 * Linesegment-factory.
 * 
 * Usage:
 *  (make-linesegment from to) -> path
 */
SCM PathFactory::make_linesegment(SCM s_from, SCM s_to) {
    char* proc = "make-linesegment";
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
SCM PathFactory::make_spiral(SCM s_path, SCM s_radius, SCM s_windings, SCM s_offset) {
    char* proc = "make-spiral";
    Path* path = scm2path(s_path, proc, 1);
    double radius = scm_num2double(s_radius,2,proc);
    double windings = scm_num2double(s_windings,3,proc);
    double offset = scm_num2double(s_offset,4,proc);
    return path2scm(new Spiral(path,radius,windings,offset));
}

SCM PathFactory::make_bezierspline(SCM s_vector_vector) {
    vector<Vector> vectors = scm2vectorlist(s_vector_vector, "make-bezierspline",1);
    return path2scm(new BezierSpline(vectors));
}

SCM PathFactory::make_catmullrom_spline(SCM s_vector_vector) {
    vector<Vector> vectors = scm2vectorlist(s_vector_vector, "make-catmullrom-spline",1);
    return path2scm(new CatmullRomSpline(vectors));
}


/**
 * Get point on path.
 *
 * Usage:
 * (point-on-path path t) -> vector
 */
SCM PathFactory::point_on_path(SCM s_path, SCM s_t) {
    Path* path = scm2path(s_path, "point-on-path", 1);
    double t = scm_num2double(s_t, 2, "point-on-path");
    Vector v = path->getPoint(t);
    return vector2scm(v);
}

/**
 * Get tangent to path.
 *
 * Usage:
 * (tangent-on-path path t) -> vector
 */
SCM PathFactory::tangent_to_path(SCM s_path, SCM s_t) {
    Path* path = scm2path(s_path, "tangent-to-path", 1);
    double t = scm_num2double(s_t, 2, "tangent-to-path");
    Vector v = path->getTangent(t);
    return vector2scm(v);
}

void PathFactory::register_procs() {
    scm_c_define_gsubr("make-circle",3,0,0,(SCM (*)()) PathFactory::make_circle);
    scm_c_define_gsubr("make-ellipse",4,0,0,(SCM (*)()) PathFactory::make_ellipse);
    scm_c_define_gsubr("make-linesegment",2,0,0,(SCM (*)()) PathFactory::make_linesegment);
    scm_c_define_gsubr("make-spiral",4,0,0,(SCM (*)()) PathFactory::make_spiral);
    scm_c_define_gsubr("make-bezierspline",1,0,0,(SCM (*)()) PathFactory::make_bezierspline);
    scm_c_define_gsubr("make-catmullrom-spline",1,0,0,(SCM (*)()) PathFactory::make_catmullrom_spline);
    scm_c_define_gsubr("point-on-path",2,0,0,(SCM (*)()) PathFactory::point_on_path);
    scm_c_define_gsubr("tangent-to-path",2,0,0,(SCM (*)()) PathFactory::point_on_path);
}
