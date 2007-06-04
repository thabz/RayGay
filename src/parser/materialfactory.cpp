
#include "parser/materialfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "parser/schemenormalperturber.h"
#include "materials/material.h"

SchemeObject* MaterialFactory::make_material(SchemeObject* s_options) {
    Material* material = new Material();

    assert(scm2bool(s_list_p (s_options)));
    uint32_t length = scm2int(s_length(s_options),0,"");
    
    assert(length % 2 == 0);
    uint32_t argc = length / 2;

    for(uint32_t i = 0; i < argc; i++) {
	size_t l;
	char* key_c = gh_symbol2newstr(s_list_ref(s_options, int2scm(i*2)),&l);
	string key = string(key_c);
	SchemeObject* s_value = s_list_ref(s_options, int2scm(i*2+1));
	if (key == "diffuse") {
	    if (isTexture(s_value)) {
		Texture* texture = scm2texture(s_value,"",0);
		material->setDiffuseTexture(texture);
	    } else {
		RGB c = scm2rgb(s_value);
		material->setDiffuseColor(c);
	    }
	} else if (key == "specular") {
	    RGB c = scm2rgb(s_value);
	    material->setSpecularColor(c);
	} else if (key == "ks") {
	    double d = safe_scm2double(s_value,0,"");
	    material->setKs(d);
	} else if (key == "kt") {
	    double d = safe_scm2double(s_value,0,"");
	    material->setKt(d);
	} else if (key == "kd") {
	    double d = safe_scm2double(s_value,0,"");
	    material->setKd(d);
	} else if (key == "eta") {
	    double d = safe_scm2double(s_value,0,"");
	    material->setEta(d);
    	} else if (key == "gloss") {
    	    assert(scm2bool(s_list_p (s_value)));
            assert(scm2int(s_length(s_value),0,"") == 2);
            SchemeObject* s_rays = s_list_ref(s_value, int2scm(0));
            uint32_t rays = scm2int(s_rays,0,"");
  	    SchemeObject* s_angle = s_list_ref(s_value, int2scm(1));
            double angle = safe_scm2double(s_angle,0,"");
    	    material->enableGloss(rays,angle);
	} else if (key == "normal") {
	    SchemeNormalPerturber* perturber = new SchemeNormalPerturber(s_value);
	    material->setNormalPerturber(perturber);
	} else if (key == "specpow") {
	    int d = scm2int(s_value,0,"");
	    material->setSc(d);
	} else {
	    cout << "Unknown material option ignored: " << key << endl;
	}
    }
    return material2scm(material);
}

void MaterialFactory::register_procs() {
    scheme->assign("make-material",1,0,0,(SchemeObject* (*)()) MaterialFactory::make_material);
}

