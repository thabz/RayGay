
#include <libguile.h>

#include "parser/parser.h"
#include "parser/wrapper.h"
#include "parser/converters.h"

#include "parser/pathfactory.h"
#include "parser/texturefactory.h"
#include "parser/materialfactory.h"
#include "parser/sceneobjectfactory.h"
#include "parser/lightsourcefactory.h"
#include "parser/camerafactory.h"
#include "parser/transformationfactory.h"
#include "parser/mathfactory.h"
#include "parser/schemefunctions.h"

#include "scene.h"
#include "renderersettings.h"

using namespace std;

Parser::Parser(string filename) {
    this->filename = filename; 
    scm_init_guile();
    init_wrapper_type();
    // Globals
    scm_c_define("scene", SCM_EOL);
    scm_c_define("camera", SCM_EOL);
    scm_c_define("renderer", SCM_EOL);
    scm_c_define("image-size", SCM_EOL);
    scm_c_define("background", SCM_EOL);

    // My procedures
    PathFactory::register_procs();
    TextureFactory::register_procs();
    MaterialFactory::register_procs();
    SceneObjectFactory::register_procs();
    LightsourceFactory::register_procs();
    CameraFactory::register_procs();
    TransformationFactory::register_procs();
    MathFactory::register_procs();
    SchemeFunctions::register_procs();
}

void Parser::run() {
    char original_working_dir[1024];

    // Change cwd to this files parent folder
    getcwd(original_working_dir,1024);
    string original_cwds = string(original_working_dir);
    string cwd = string(original_working_dir) + "/" + filename;
    string filename_clean = string(cwd);
    int idx = cwd.find_last_of('/');
    cwd.resize(idx);
    filename_clean = filename_clean.substr(idx+1, filename_clean.length());
    chdir(cwd.c_str());

    scm_c_primitive_load(filename_clean.c_str());

    chdir(original_working_dir);
}

SCM Parser::lookup(string var_name) {
	SCM var = scm_c_lookup(var_name.c_str());
	return scm_variable_ref(var);
}

void Parser::populate(Scene* scene, RendererSettings* renderersettings) {
    // Populate sceneobjects and lights
    SCM list = lookup("scene");
    assert(SCM_NFALSEP (scm_list_p (list)));
    uint length = scm_num2int(scm_length(list),0,"internal-populate-scene");

    //cout << "Scene objects: " << length << endl;

    for(uint i = 0; i < length; i++) {
	SCM s_value = scm_list_ref(list, scm_int2num(i));
	//assert(!SCM_NFALSEP (scm_list_p (s_value)));
	if (isSceneObject(s_value)) {
	    //cout << "Found a scene object" << endl;
	    SceneObject* sceneobject = scm2sceneobject(s_value, "internal-populate-scene", 0);
	    scene->addObject(sceneobject);
	} else if (isLightsource(s_value)) {
	    //cout << "Found a lightsource" << endl;
	    Lightsource* light = scm2lightsource(s_value, "internal-populate-scene", 0);
	    scene->addLight(light);
	} else {
	    scm_error(NULL, "internal-populating-scene", "A non-sceneobject or non-lightsource found.", SCM_UNSPECIFIED, NULL);
	}
    }

    // Get renderer
    SCM s_renderer = lookup("renderer");
    RendererSettings::RendererType type;
    if (!SCM_NULLP(s_renderer)) {
	string r_string = scm2string(s_renderer);
	if (r_string == "raytracer") {
	    type = RendererSettings::RAYTRACER;
	} else if (r_string == "photontracer") {
	    type = RendererSettings::RAYTRACER;
	} else if (r_string == "pathtracer") {
	    type = RendererSettings::RAYTRACER;
	} else {
	    type = RendererSettings::NONE;
	    scm_error(NULL, "internal-setting-renderer", ("Unknown renderertype: " + r_string).c_str(), SCM_UNSPECIFIED, NULL);
	} 
    } else {
	type = RendererSettings::NONE;
    }
    renderersettings->renderertype = type;

    // Populate camera
    SCM s_camera = lookup("camera");
    if (!SCM_NULLP(s_camera)) {
	Camera* camera = scm2camera(s_camera, "internal-get-camera", 0);
	scene->setCamera(camera);
    } else {
	if (renderersettings->renderertype != RendererSettings::NONE) {
	    cout << "No camera defined. Disabled rendering." << endl;
	    renderersettings->renderertype = RendererSettings::NONE;
	}
    }

    SCM s_image_size = lookup("image-size");
    if (!SCM_NULLP(s_image_size)) {
	assert(SCM_NFALSEP (scm_list_p (s_image_size)));
	assert(scm_num2int(scm_length(s_image_size),0,"") == 2);
	SCM s_w = scm_list_ref(s_image_size, scm_int2num(0));
	uint w = scm_num2int(s_w,0,"");
	SCM s_h = scm_list_ref(s_image_size, scm_int2num(1));
	uint h = scm_num2int(s_h,0,"");
	renderersettings->image_width = w;
	renderersettings->image_height = h;
    }

    SCM s_background = lookup("background");
    if (!SCM_NULLP(s_background)) {
	char* subr = "internal: setting scene background";
	if (isWrappedObject(s_background)) {
	    Texture* texture = scm2texture(s_background, subr, 0);
	    scene->setBackground(texture);
	} else {
	    RGBA rgba = scm2rgba(s_background, subr, 0);
	    scene->setBackground(rgba);
	}
    }

    // TODO: Set fog
}

