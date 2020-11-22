
#ifndef PARSER_PATH_FACTORY_H
#define PARSER_PATH_FACTORY_H

#include "scheme/scheme.h"

/**
 * Factory for path-related Scheme-procedures.
 */
class PathFactory {

public:
  static SchemeObject *make_circle(Scheme *scheme, SchemeObject *s_center,
                                   SchemeObject *s_radius,
                                   SchemeObject *s_normal);

  static SchemeObject *make_ellipse(Scheme *scheme, SchemeObject *s_center,
                                    SchemeObject *s_radus_x,
                                    SchemeObject *s_radius_y,
                                    SchemeObject *s_normal);

  static SchemeObject *make_linesegment(Scheme *scheme, SchemeObject *s_from,
                                        SchemeObject *s_to);
  static SchemeObject *make_spiral(Scheme *scheme, SchemeObject *s_path,
                                   SchemeObject *s_radius,
                                   SchemeObject *s_windings,
                                   SchemeObject *s_offset);

  static SchemeObject *make_bezierspline(Scheme *scheme,
                                         SchemeObject *s_vector_vector);
  static SchemeObject *make_catmullrom_spline(Scheme *scheme,
                                              SchemeObject *s_vector_vector);

  static SchemeObject *point_on_path(Scheme *scheme, SchemeObject *s_path,
                                     SchemeObject *s_t);

  static SchemeObject *tangent_to_path(Scheme *scheme, SchemeObject *s_path,
                                       SchemeObject *s_t);

  static void register_procs(Scheme *scheme);
};

#endif
