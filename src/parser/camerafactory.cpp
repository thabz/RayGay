
#include <iostream>

#include "parser/camerafactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "cameras/camera.h"
#include "cameras/pinhole.h"

using namespace std;

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
SCM CameraFactory::make_pinhole_camera(SCM s_options) {
    Camera* camera = new Pinhole();

    assert(SCM_NFALSEP (scm_list_p (s_options)));
    uint length = scm_num2int(scm_length(s_options),0,"");
    
    assert(length % 2 == 0);
    uint argc = length / 2;

    for(uint i = 0; i < argc; i++) {
	size_t l;
	char* key_c = gh_symbol2newstr(scm_list_ref(s_options, scm_int2num(i*2)),&l);
	string key = string(key_c);
	SCM s_value = scm_list_ref(s_options, scm_int2num(i*2+1));
	if (key == "pos") {
	    Vector v = scm2vector(s_value," make-pinhole-camera", 2+2*i);
	    camera->setPosition(v);
	} else if (key == "lookat") {
	    Vector v = scm2vector(s_value, "make-pinhole-camera", 2+2*i);
	    camera->setLookAt(v);
	} else if (key == "up") {
	    Vector v = scm2vector(s_value, "make-pinhole-camera", 2+2*i);
	    camera->setUp(v);
	} else if (key == "up") {
	    Vector v = scm2vector(s_value, "make-pinhole-camera", 2+2*i);
	    camera->setUp(v);
	} else if (key == "fov") {
	    double fov = scm_num2double(s_value,0,"");
	    camera->setFieldOfView(fov);
	} else if (key == "aa") {
	    int aa = scm_num2int(s_value,0,"");
	    camera->enableAdaptiveSupersampling(aa);
	} else if (key == "dof") {
	    SCM scms[3];
	    for(uint i = 0; i < 3; i++) {
		scms[i] = scm_list_ref(s_value, scm_int2num(i));
	    }
	    double aperture = scm_num2double(scms[0], 0, "");
	    int samples = scm_num2int(scms[1], 0, "");
	    Vector focalpoint = scm2vector(scms[2], "", 0);
	    camera->enableDoF(aperture, samples, focalpoint);
	} else {
	    cout << "Unknown camera option: " << key << endl;
	}
    }
    return camera2scm(camera);
}


void CameraFactory::register_procs() {
    scm_c_define_gsubr("make-pinhole-camera",1,0,0,(SCM (*)()) CameraFactory::make_pinhole_camera);
}
