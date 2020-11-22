
#include <iostream>

#include "cameras/camera.h"
#include "cameras/fisheye.h"
#include "cameras/latlong.h"
#include "cameras/pinhole.h"
#include "parser/camerafactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "samplers/boundary_adaptive.h"
#include "samplers/halton_sampler.h"
#include "samplers/non_aa_sampler.h"
#include "samplers/uniform_jitter.h"
#include "samplers/whitted_adaptive.h"

using namespace std;

SchemeObject *CameraFactory::s_camera_p(Scheme *scheme, SchemeObject *obj) {
  return isWrappedObjectType(obj, CAMERA);
}

void extractCamera(Scheme *scheme, SchemeObject *s_options, Camera *camera,
                   const wchar_t *function_name) {
  if (!scm2bool(s_list_p(scheme, s_options))) {
    wrong_type_arg(function_name, 1, s_options);
  }
  uint32_t length = safe_scm2int(s_length(scheme, s_options), 0, L"");

  assert(length % 2 == 0);
  uint32_t argc = length / 2;

  bool fast_preview = RendererSettings::uniqueInstance()->fast_preview;

  for (uint32_t i = 0; i < argc; i++) {
    SchemeObject *s_key = s_list_ref(scheme, s_options, int2scm(i * 2));
    if (i_symbol_p(s_key) == S_FALSE) {
      throw scheme_exception(L"Invalid camera-option-name: " +
                             s_key->toString());
    }
    wstring key = s_key->toString();
    SchemeObject *s_value = s_list_ref(scheme, s_options, int2scm(i * 2 + 1));
    if (key == L"pos") {
      Vector v = scm2vector(s_value, function_name, 2 + 2 * i);
      camera->setPosition(v);
    } else if (key == L"lookat") {
      Vector v = scm2vector(s_value, function_name, 2 + 2 * i);
      camera->setLookAt(v);
    } else if (key == L"up") {
      Vector v = scm2vector(s_value, function_name, 2 + 2 * i);
      camera->setUp(v);
    } else if (key == L"fov") {
      double fov = safe_scm2double(s_value, 0, L"");
      camera->setFieldOfView(fov);
    } else if (key == L"aa" && !fast_preview) {
      int aa = safe_scm2int(s_value, 0, L"");
      SamplerFactory *s = new WhittedAdaptiveFactory(aa);
      camera->setSamplerFactory(s);
      camera->enableAdaptiveSupersampling(aa);
    } else if (key == L"sampler" && !fast_preview) {
      SamplerFactory *s = scm2sampler(s_value, function_name, 2 + 2 * i);
      camera->setSamplerFactory(s);
    } else if (key == L"dof" && !fast_preview) {
      SchemeObject *scms[3];
      for (uint32_t i = 0; i < 3; i++) {
        scms[i] = s_list_ref(scheme, s_value, int2scm(i));
      }
      double aperture = safe_scm2double(scms[0], 0, L"");
      int samples = safe_scm2int(scms[1], 0, L"");
      Vector focalpoint = scm2vector(scms[2], L"", 0);
      camera->enableDoF(aperture, samples, focalpoint);
    } else if (key == L"zoom") {
      SchemeObject *scms[2];
      scms[0] = s_list_ref(scheme, s_value, int2scm(0));
      scms[1] = s_list_ref(scheme, s_value, int2scm(1));
      Vector2 offset = scm2vector2(scms[0], L"scm2vector2", 0);
      double width = safe_scm2double(scms[1], 0, L"num2double");
      camera->setZoom(offset, width);
    } else if (key == L"sampler" && fast_preview) {
      cout << "Ignoring Sampler setting because of fast preview." << endl;
    } else if (key == L"aa" && fast_preview) {
      cout << "Ignoring AA setting because of fast preview." << endl;
    } else if (key == L"dof" && fast_preview) {
      cout << "Ignoring depth-of-field setting because of fast preview."
           << endl;
    } else {
      wcout << L"Unknown camera option: " << key << endl;
    }
  }

  if (camera->getSamplerFactory() == NULL) {
    SamplerFactory *p = new NonAASamplerFactory();
    camera->setSamplerFactory(p);
  }
}

/**
 * Create a pinhole camera.
 *
 * Usage:
 *
 *  (make-pinhole-camera
 *   '( pos (-2700 2700 20)
 *      lookat (0 -200 0)
 *      up (0 1 0)
 *      fov 45
 *      dof (150.0 30 (-750 0 0))
 *      aa 0))
 */
SchemeObject *CameraFactory::s_make_pinhole_camera(Scheme *scheme,
                                                   SchemeObject *s_options) {
  Camera *camera = new Pinhole();
  extractCamera(scheme, s_options, camera, L"make-pinhole-camera");
  return camera2scm(camera);
}

SchemeObject *CameraFactory::s_make_lat_long_camera(Scheme *scheme,
                                                    SchemeObject *s_options) {
  Camera *camera = new LatLong();
  extractCamera(scheme, s_options, camera, L"make-lat-long-camera");
  return camera2scm(camera);
}

SchemeObject *CameraFactory::s_make_fisheye_camera(Scheme *scheme,
                                                   SchemeObject *s_options) {
  Camera *camera = new Fisheye();
  extractCamera(scheme, s_options, camera, L"make-fisheye-camera");
  return camera2scm(camera);
}

SchemeObject *
CameraFactory::s_make_whitted_adaptive_sampler(Scheme *scheme,
                                               SchemeObject *s_aa_depth) {
  const wchar_t *proc = L"make-whitted-adaptive-sampler";
  assert_arg_non_negative_int(proc, 1, s_aa_depth);
  int aa_depth = scm2int(s_aa_depth);
  SamplerFactory *sampler = new WhittedAdaptiveFactory(aa_depth);
  return sampler2scm(sampler);
}

/*
SchemeObject* CameraFactory::s_make_boundary_adaptive_sampler(Scheme* scheme,
SchemeObject* s_aa_depth)
{
    const wchar_t* proc = L"make-boundary-adaptive-sampler";
    assert_arg_non_negative_int(proc, 1, s_aa_depth);
    int aa_depth = scm2int(s_aa_depth);
    SamplerFactory* sampler = new BoundaryAdaptiveFactory(aa_depth);
    return sampler2scm(sampler);
}
*/

SchemeObject *
CameraFactory::s_make_uniform_jitter_sampler(Scheme *scheme,
                                             SchemeObject *s_samples_num) {
  const wchar_t *proc = L"make-uniform-jitter-sampler";
  assert_arg_positive_int(proc, 1, s_samples_num);
  int samples_num = scm2int(s_samples_num);
  SamplerFactory *sampler = new UniformJitterFactory(samples_num);
  return sampler2scm(sampler);
}

SchemeObject *
CameraFactory::s_make_halton_sampler(Scheme *scheme,
                                     SchemeObject *s_samples_num) {
  const wchar_t *proc = L"make-halton-sampler";
  assert_arg_positive_int(proc, 1, s_samples_num);
  int samples_num = scm2int(s_samples_num);
  SamplerFactory *sampler = new HaltonSamplerFactory(samples_num);
  return sampler2scm(sampler);
}

SchemeObject *CameraFactory::s_sampler_p(Scheme *scheme, SchemeObject *object) {
  return isWrappedObjectType(object, SAMPLER);
}

void CameraFactory::register_procs(Scheme *scheme) {
  scheme->assign(L"camera?", 1, 0, 0,
                 (SchemeObject * (*)()) CameraFactory::s_camera_p);
  scheme->assign(L"make-pinhole-camera", 1, 0, 0,
                 (SchemeObject * (*)()) CameraFactory::s_make_pinhole_camera);
  scheme->assign(L"make-lat-long-camera", 1, 0, 0,
                 (SchemeObject * (*)()) CameraFactory::s_make_lat_long_camera);
  scheme->assign(L"make-fisheye-camera", 1, 0, 0,
                 (SchemeObject * (*)()) CameraFactory::s_make_fisheye_camera);
  scheme->assign(L"sampler?", 1, 0, 0, (SchemeObject * (*)()) s_sampler_p);
  scheme->assign(L"make-whitted-adaptive-sampler", 1, 0, 0,
                 (SchemeObject * (*)())
                     CameraFactory::s_make_whitted_adaptive_sampler);
  //   scheme->assign("make-boundary-adaptive-sampler",1,0,0,(SchemeObject*
  //   (*)()) CameraFactory::make_boundary_adaptive_sampler);
  scheme->assign(L"make-uniform-jitter-sampler", 1, 0, 0,
                 (SchemeObject * (*)())
                     CameraFactory::s_make_uniform_jitter_sampler);
  scheme->assign(L"make-halton-sampler", 1, 0, 0,
                 (SchemeObject * (*)()) CameraFactory::s_make_halton_sampler);
}
