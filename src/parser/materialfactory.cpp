
#include "parser/materialfactory.h"
#include "parser/texturefactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "parser/schemenormalperturber.h"
#include "materials/material.h"

Scheme* MaterialFactory::scheme;


SchemeObject* s_material_p(SchemeObject* object) {
    return isWrappedObjectType(object, MATERIAL);        
}

SchemeObject* MaterialFactory::s_make_material(SchemeObject* s_options) {
    Material* material = new Material();

    assert(scm2bool(s_list_p (s_options)));
    uint32_t length = safe_scm2int(s_length(s_options),0,"");
    
    assert(length % 2 == 0);
    uint32_t argc = length / 2;

    for(uint32_t i = 0; i < argc; i++) {
        SchemeObject* s_key = s_list_ref(s_options, int2scm(i*2));
	SchemeObject* s_value = s_list_ref(s_options, int2scm(i*2+1));
        if (i_symbol_p(s_key) == S_FALSE) {
            throw scheme_exception("Invalid camera-option-name: " + s_key->toString());
        }    
    	string key = s_key->toString();

	if (key == "diffuse") {
	    if (s_texture_p(s_value) == S_TRUE) {
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
            assert(safe_scm2int(s_length(s_value),0,"") == 2);
            SchemeObject* s_rays = s_list_ref(s_value, int2scm(0));
            uint32_t rays = safe_scm2int(s_rays,0,"");
  	    SchemeObject* s_angle = s_list_ref(s_value, int2scm(1));
            double angle = safe_scm2double(s_angle,0,"");
    	    material->enableGloss(rays,angle);
	} else if (key == "normal") {
	    SchemeNormalPerturber* perturber = new SchemeNormalPerturber(scheme, s_value);
	    material->setNormalPerturber(perturber);
	} else if (key == "specpow") {
	    int d = safe_scm2int(s_value,0,"");
	    material->setSc(d);
	} else {
	    cout << "Unknown material option ignored: " << key << endl;
	}
    }
    return material2scm(material);
}

void MaterialFactory::register_procs(Scheme* s) {
    scheme = s;        
    scheme->assign("material?",1,0,0,(SchemeObject* (*)()) s_material_p);
    scheme->assign("make-material",1,0,0,(SchemeObject* (*)()) MaterialFactory::s_make_material);
}

