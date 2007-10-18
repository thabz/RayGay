
#include <iostream>

#include "parser/camerafactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "cameras/camera.h"
#include "cameras/pinhole.h"
#include "cameras/fisheye.h"
#include "cameras/latlong.h"
#include "samplers/non_aa_sampler.h"
#include "samplers/whitted_adaptive.h"
#include "samplers/boundary_adaptive.h"
#include "samplers/uniform_jitter.h"
#include "samplers/halton_sampler.h"

using namespace std;

void extractCamera(SchemeObject* s_options, Camera* camera, char* function_name) {
    if (!scm2bool(s_list_p (s_options))) {
	wrong_type_arg (function_name, 1, s_options);
    }
    uint32_t length = safe_scm2int(s_length(s_options),0,"");
    
    assert(length % 2 == 0);
    uint32_t argc = length / 2;

    bool fast_preview = RendererSettings::uniqueInstance()->fast_preview;
    
    for(uint32_t i = 0; i < argc; i++) {
        SchemeObject* s_key = s_list_ref(s_options, int2scm(i*2));
        if (i_symbol_p(s_key) == S_FALSE) {
            throw scheme_exception("Invalid camera-option-name: " + s_key->toString());
        }    
	string key = s_key->toString();
	SchemeObject* s_value = s_list_ref(s_options, int2scm(i*2+1));
	if (key == "pos") {
	    Vector v = scm2vector(s_value, function_name, 2+2*i);
	    camera->setPosition(v);
	} else if (key == "lookat") {
	    Vector v = scm2vector(s_value, function_name, 2+2*i);
	    camera->setLookAt(v);
	} else if (key == "up") {
	    Vector v = scm2vector(s_value, function_name, 2+2*i);
	    camera->setUp(v);
	} else if (key == "fov") {
	    double fov = safe_scm2double(s_value,0,"");
	    camera->setFieldOfView(fov);
	} else if (key == "aa" && !fast_preview) {
	    int aa = safe_scm2int(s_value,0,"");
	    SamplerFactory* s = new WhittedAdaptiveFactory(aa);
	    camera->setSamplerFactory(s);
	    camera->enableAdaptiveSupersampling(aa);
	} else if (key == "sampler" && !fast_preview) {
	    SamplerFactory* s = scm2sampler(s_value, function_name, 2+2*i);
	    camera->setSamplerFactory(s);
	} else if (key == "dof" && !fast_preview) {
	    SchemeObject* scms[3];
	    for(uint32_t i = 0; i < 3; i++) {
		scms[i] = s_list_ref(s_value, int2scm(i));
	    }
	    double aperture = safe_scm2double(scms[0], 0, "");
	    int samples = safe_scm2int(scms[1], 0, "");
	    Vector focalpoint = scm2vector(scms[2], "", 0);
            camera->enableDoF(aperture, samples, focalpoint);
    	} else if (key == "zoom") {
    	    SchemeObject* scms[2];
 	    scms[0] = s_list_ref(s_value, int2scm(0));
 	    scms[1] = s_list_ref(s_value, int2scm(1));
            Vector2 offset = scm2vector2(scms[0], "scm2vector2", 0);
    	    double width = safe_scm2double(scms[1], 0, "num2double");
    	    camera->setZoom(offset, width);
	} else if (key == "sampler" && fast_preview) {
	    cout << "Ignoring Sampler setting because of fast preview." << endl;    
    	} else if (key == "aa" && fast_preview) {
    	    cout << "Ignoring AA setting because of fast preview." << endl;
    	} else if (key == "dof" && fast_preview) {
    	    cout << "Ignoring depth-of-field setting because of fast preview." << endl;
	} else {        
	    cout << "Unknown camera option: " << key << endl;
	}
    }
    
    if (camera->getSamplerFactory() == NULL) {
	SamplerFactory* p = new NonAASamplerFactory();
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
SchemeObject* CameraFactory::make_pinhole_camera(SchemeObject* s_options) {
    Camera* camera = new Pinhole();
    extractCamera(s_options, camera, "make-pinhole-camera");
    return camera2scm(camera);
}

SchemeObject* CameraFactory::make_lat_long_camera(SchemeObject* s_options) {
    Camera* camera = new LatLong();
    extractCamera(s_options, camera, "make-lat-long-camera");
    return camera2scm(camera);
}

SchemeObject* CameraFactory::make_fisheye_camera(SchemeObject* s_options) {
    Camera* camera = new Fisheye();
    extractCamera(s_options, camera, "make-fisheye-camera");
    return camera2scm(camera);
}

SchemeObject* CameraFactory::make_whitted_adaptive_sampler(SchemeObject* s_aa_depth)
{
    char* proc = "make-whitted-adaptive-sampler";
    int aa_depth = safe_scm2int(s_aa_depth, 1, proc);
    SamplerFactory* sampler = new WhittedAdaptiveFactory(aa_depth);
    return sampler2scm(sampler);
}

/*
SchemeObject* CameraFactory::make_boundary_adaptive_sampler(SchemeObject* s_aa_depth)
{
    char* proc = "make-boundary-adaptive-sampler";
    int aa_depth = safe_scm2int(s_aa_depth, 1, proc);
    SamplerFactory* sampler = new BoundaryAdaptiveFactory(aa_depth);
    return sampler2scm(sampler);
}
*/

SchemeObject* CameraFactory::make_uniform_jitter_sampler(SchemeObject* s_samples_sqrt) 
{
    char* proc = "make-uniform-jitter-sampler";
    int samples_sqrt = safe_scm2int(s_samples_sqrt, 1, proc);
    SamplerFactory* sampler = new UniformJitterFactory(samples_sqrt);
    return sampler2scm(sampler);
}

SchemeObject* CameraFactory::make_halton_sampler(SchemeObject* s_samples_num) 
{
    char* proc = "make-halton-sampler";
    int samples_num = safe_scm2int(s_samples_num, 1, proc);
    SamplerFactory* sampler = new HaltonSamplerFactory(samples_num);
    return sampler2scm(sampler);
}

void CameraFactory::register_procs(Scheme* scheme) {
    scheme->assign("make-pinhole-camera",1,0,0,(SchemeObject* (*)()) CameraFactory::make_pinhole_camera);
    scheme->assign("make-lat-long-camera",1,0,0,(SchemeObject* (*)()) CameraFactory::make_lat_long_camera);
    scheme->assign("make-fisheye-camera",1,0,0,(SchemeObject* (*)()) CameraFactory::make_fisheye_camera);
    scheme->assign("make-whitted-adaptive-sampler",1,0,0,(SchemeObject* (*)()) CameraFactory::make_whitted_adaptive_sampler);
 //   scheme->assign("make-boundary-adaptive-sampler",1,0,0,(SchemeObject* (*)()) CameraFactory::make_boundary_adaptive_sampler);
    scheme->assign("make-uniform-jitter-sampler",1,0,0,(SchemeObject* (*)()) CameraFactory::make_uniform_jitter_sampler);
    scheme->assign("make-halton-sampler",1,0,0,(SchemeObject* (*)()) CameraFactory::make_halton_sampler);
}
