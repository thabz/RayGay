
#include "parser/materialfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "parser/schemenormalperturber.h"
#include "materials/material.h"

SCM MaterialFactory::make_material(SCM s_options) {
    Material* material = new Material();

    assert(SCM_NFALSEP (scm_list_p (s_options)));
    uint length = scm_num2int(scm_length(s_options),0,"");
    
    assert(length % 2 == 0);
    uint argc = length / 2;

    for(uint i = 0; i < argc; i++) {
	size_t l;
	char* key_c = gh_symbol2newstr(scm_list_ref(s_options, scm_int2num(i*2)),&l);
	string key = string(key_c);
	SCM s_value = scm_list_ref(s_options, scm_int2num(i*2+1));
	if (key == "diffuse") {
	    RGB c = scm2rgb(s_value);
	    material->setDiffuseColor(c);
	} else if (key == "specular") {
	    RGB c = scm2rgb(s_value);
	    material->setSpecularColor(c);
	} else if (key == "ks") {
	    double d = scm_num2double(s_value,0,"");
	    material->setKs(d);
	} else if (key == "kt") {
	    double d = scm_num2double(s_value,0,"");
	    material->setKt(d);
	} else if (key == "kd") {
	    double d = scm_num2double(s_value,0,"");
	    material->setKd(d);
	} else if (key == "normal") {
	    SchemeNormalPerturber* perturber = new SchemeNormalPerturber(s_value);
	    material->setNormalPerturber(perturber);
	} else if (key == "specpow") {
	    int d = scm_num2int(s_value,0,"");
	    material->setSc(d);
	} else {
	    cout << "Unknown material option ignored: " << key << endl;
	}
    }
    return material2scm(material);
}

void MaterialFactory::register_procs() {
    scm_c_define_gsubr("make-material",1,0,0,(SCM (*)()) MaterialFactory::make_material);
}

