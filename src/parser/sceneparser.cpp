
#include <sstream>

#include "parser/sceneparser.h"
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

char* VAR_SCENE = "__scene__";
char* VAR_CAMERA = "__camera__";
char* VAR_RENDERER = "__renderer__";
char* VAR_IMAGESIZE = "__image-size__";
char* VAR_BACKGROUND = "__background__";

SceneParser::SceneParser() {
    scheme = new Scheme();        
    init_wrapper_type();
    
    // Globals
    scheme->assign(VAR_SCENE, S_EMPTY_LIST);
    scheme->assign(VAR_CAMERA, S_EMPTY_LIST);
    scheme->assign(VAR_RENDERER, S_EMPTY_LIST);
    scheme->assign(VAR_IMAGESIZE, S_EMPTY_LIST);
    scheme->assign(VAR_BACKGROUND, S_EMPTY_LIST);

    scheme->assign("set-settings",1,0,0,(SCM (*)()) SceneParser::set_settings);

    // My procedures
    PathFactory::register_procs(scheme);
    TextureFactory::register_procs(scheme);
    MaterialFactory::register_procs(scheme);
    SceneObjectFactory::register_procs(scheme);
    LightsourceFactory::register_procs(scheme);
    CameraFactory::register_procs(scheme);
    TransformationFactory::register_procs(scheme);
    MathFactory::register_procs(scheme);
    SchemeFunctions::register_procs(scheme);
}

void SceneParser::assignVariable(string var, double value) {
    scheme->assign(var.c_str(), s_double2scm(value));
}

void SceneParser::parse_expr(std::string expr) {
    scheme->eval(expr);
}

void SceneParser::parse_file(std::string filename) {
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

    ifstream* ifs = new ifstream(filename_clean.c_str(), ios::in);
    if (ifs->fail()) {
        cout << "Error opening file" << endl;
        exit(EXIT_FAILURE);
    }
    try {
        scheme->eval(ifs);
    } catch (scheme_exception e) {
        ifs->close();
	cerr << "ABORT: " << e.toString() << endl;
        exit(EXIT_FAILURE);
    }
    ifs->close();

    chdir(original_working_dir);
}

SCM SceneParser::lookup(string var_name) {
    return scheme->lookup(var_name);        
}

void SceneParser::populate(Scene* scene, RendererSettings* renderersettings) {
    // Populate sceneobjects and lights
    SCM list = lookup(VAR_SCENE);
    if (list == NULL || S_FALSE == s_list_p(list)) {
	throw scheme_exception("internal-populate-scene", "The variable '"+VAR_SCENE+"' is not a list");
    }
    uint32_t length = scm2int(s_length(list),0,"internal-populate-scene");

    //cout << "Scene objects: " << length << endl;

    for(uint32_t i = 0; i < length; i++) {
	SCM s_value = s_list_ref(list, int2scm(i));
	//assert(!S_TRUE == (s_list_p (s_value)));
	if (isSceneObject(s_value)) {
	    //cout << "Found a scene object" << endl;
	    SceneObject* sceneobject = scm2sceneobject(s_value, "internal-populate-scene", 0);
	    scene->addObject(sceneobject);
	} else if (isLightsource(s_value)) {
	    //cout << "Found a lightsource" << endl;
	    Lightsource* light = scm2lightsource(s_value, "internal-populate-scene", 0);
	    scene->addLight(light);
	} else {
	    throw scheme_exception("internal-populating-scene", "A non-sceneobject or non-lightsource found.");
	}
    }

    // Get renderer
    SCM s_renderer = lookup(VAR_RENDERER);
    RendererSettings::RendererType type;
    if (s_renderer != NUL) {
	string r_string = scm2string(s_renderer);
	if (r_string == "raytracer") {
	    type = RendererSettings::RAYTRACER;
	} else if (r_string == "photonrenderer") {
	    type = RendererSettings::PHOTON_RENDERER;
	} else if (r_string == "pathtracer") {
	    type = RendererSettings::PATHTRACER;
	} else if (r_string == "none") {
    	    type = RendererSettings::NONE;
	} else {
	    type = RendererSettings::NONE;
	    throw scheme_exception("internal-setting-renderer", "Unknown renderertype: " + r_string);
	} 
    } else {
	type = RendererSettings::NONE;
    }
    renderersettings->renderertype = type;
    
    if (renderersettings->fast_preview) {
        type = RendererSettings::RAYTRACER;
    }

    // Populate camera
    SCM s_camera = lookup(VAR_CAMERA);
    if (s_camera != NULL) {
	Camera* camera = scm2camera(s_camera, "internal-get-camera", 0);
	scene->setCamera(camera);
    } else {
	if (renderersettings->renderertype != RendererSettings::NONE) {
	    cout << "No camera defined. Disabled rendering." << endl;
	    renderersettings->renderertype = RendererSettings::NONE;
	}
    }
    
    SCM s_image_size = lookup(VAR_IMAGESIZE);
    if (s_image_size != NULL) {
	assert(S_TRUE == (s_list_p (s_image_size)));
	assert(scm2int(s_length(s_image_size),0,"") == 2);
	SCM s_w = s_list_ref(s_image_size, S_ZERO);
	uint32_t w = scm2int(s_w,0,"");
	SCM s_h = s_list_ref(s_image_size, S_ONE);
	uint32_t h = scm2int(s_h,0,"");
	renderersettings->image_width = w;
	renderersettings->image_height = h;
    }

    SCM s_background = lookup(VAR_BACKGROUND);
    if (s_background != NULL) {
	char* subr = "internal: setting scene background";
	if (isWrappedObject(s_background)) {
	    Texture* texture = scm2texture(s_background, subr, 0);
	    scene->setBackground(texture);
	} else {
	    RGBA rgba = scm2rgba(s_background, subr, 0);
	    scene->setBackground(rgba);
	}
    }
}

// TODO: Set fog

SCM SceneParser::set_settings(SCM s_settings) 
{
    RendererSettings* renderersettings = RendererSettings::uniqueInstance();
    char* proc = "set-settings";
    if (S_FALSE == i_null_p(s_settings)) {
	if (S_FALSE == (s_list_p (s_settings))) {
	    throw scheme_exception(proc, "The settings is not a list");
	}
	uint32_t length = scm2int(s_length(s_settings),0,"");

	assert(length % 2 == 0);
	uint32_t argc = length / 2;

	for(uint32_t i = 0; i < argc; i++) {
	    char* key_c = gh_symbol2newstr(s_list_ref(s_settings, int2scm(i*2)),NULL);
	    string key = string(key_c);
	    SCM s_value = s_list_ref(s_settings, int2scm(i*2+1));
	    if (key == "globalphotons") {
		uint32_t value = scm2int(s_value,0,proc);
		renderersettings->global_photons_num = value;
	    } else if (key == "causticphotons") {
		double value = s_scm2double(s_value,0,proc);
		renderersettings->caustic_photons_num = int(value);
	    } else if (key == "estimateradius") {
		double value = s_scm2double(s_value,0,proc);
		renderersettings->estimate_radius = int(value);
	    } else if (key == "estimateradius") {
		double value = s_scm2double(s_value,0,proc);
		renderersettings->estimate_radius = value;
	    } else if (key == "estimatesamples") {
		double value = s_scm2double(s_value,0,proc);
		renderersettings->estimate_samples = int(value);
	    } else if (key == "finalgatherrays") {
		double value = s_scm2double(s_value,0,proc);
		renderersettings->final_gather_rays = int(value);
	    } else if (key == "cachetolerance") {
		double value = s_scm2double(s_value,0,proc);
		renderersettings->cache_tolerance = value;
    	    } else if (key == "fast-preview") {
    		renderersettings->fast_preview = scm2bool(s_value) ? true : false;
	    } else if (key == "image-storage") {
		string s = scm2string(s_value);
		if (s == "memory") {
		    renderersettings->image_alloc_model = Allocator::MALLOC_ONLY;    
		} else if (s == "disc") {
		    renderersettings->image_alloc_model = Allocator::MMAP_ONLY;    
		} else if (s == "auto") {
		    renderersettings->image_alloc_model = Allocator::AUTO;    
		} else {
		    throw scheme_exception(proc, "Unknown image-storage model: " + s);
		}
	    } else {
		cout << "WARNING: Unknown setting: " << key << endl;
	    }
	}
    }
    return NULL;

}

string SceneParser::version() {
    // TODO: Add VERSION        
    return "RayGay Scheme";     
}
