
#ifndef PARSER_CAMERA_FACTORY_H
#define PARSER_CAMERA_FACTORY_H

#include "scheme/scheme.h"

/**
 * Factory for camera-related Scheme-procedures.
 */
class CameraFactory {

public:
  static SchemeObject *s_camera_p(Scheme *scheme, SchemeObject *s_obj);
  static SchemeObject *s_make_pinhole_camera(Scheme *scheme,
                                             SchemeObject *s_options);
  static SchemeObject *s_make_lat_long_camera(Scheme *scheme,
                                              SchemeObject *s_options);
  static SchemeObject *s_make_fisheye_camera(Scheme *scheme,
                                             SchemeObject *s_options);

  static SchemeObject *s_sampler_p(Scheme *scheme, SchemeObject *s_obj);
  static SchemeObject *s_make_whitted_adaptive_sampler(Scheme *scheme,
                                                       SchemeObject *s_options);
  static SchemeObject *
  s_make_boundary_adaptive_sampler(Scheme *scheme, SchemeObject *s_options);
  static SchemeObject *s_make_uniform_jitter_sampler(Scheme *scheme,
                                                     SchemeObject *s_options);
  static SchemeObject *s_make_halton_sampler(Scheme *scheme,
                                             SchemeObject *s_options);

  static void register_procs(Scheme *scheme);
};

#endif
