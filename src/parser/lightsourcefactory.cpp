
#include "parser/lightsourcefactory.h"
#include "renderersettings.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "lights/arealight.h"
#include "lights/spotlight.h"
#include "lights/pointlight.h"
#include "lights/skylight.h"

SchemeObject* s_lightsource_p(SchemeObject* object) {
    return isWrappedObjectType(object, LIGHTSOURCE);        
}

SchemeObject* s_make_pointlight(SchemeObject* s_pos, SchemeObject* s_power) {
    Vector pos = scm2vector(s_pos, "make-pointlight", 1);
    Pointlight* light = new Pointlight(pos);
    if (s_power != S_UNSPECIFIED) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SchemeObject* s_make_arealight(SchemeObject* s_pos, SchemeObject* s_dir, SchemeObject* s_radius, SchemeObject* s_num, SchemeObject* s_jitter, SchemeObject* s_power) {
    
    Vector pos = scm2vector(s_pos, "make-arealight", 1);
    Vector dir = scm2vector(s_dir, "make-arealight", 2);
    double radius = safe_scm2double(s_radius,3,"make-arealight");
    double jitter = safe_scm2double(s_jitter,5,"make-arealight");
    int num = safe_scm2int(s_num,4,"make-arealight");
    if (RendererSettings::uniqueInstance()->fast_preview) {
        return s_make_pointlight(s_pos, s_power);
    }
    Arealight* light = new Arealight(pos,dir,radius,num,jitter);
    if (s_power != S_UNSPECIFIED) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SchemeObject* s_make_spotlight(SchemeObject* s_pos, SchemeObject* s_lookat, SchemeObject* s_angle, SchemeObject* s_cut_angle, SchemeObject* s_power) {
    Vector pos = scm2vector(s_pos, "make-spotlight", 1);
    Vector lookat= scm2vector(s_lookat, "make-spotlight", 2);
    double angle = safe_scm2double(s_angle,3,"make-spotlight");
    double cut_angle = safe_scm2double(s_cut_angle,4,"make-spotlight");
    Spotlight* light = new Spotlight(pos,lookat,angle,cut_angle);
    if (s_power != S_UNSPECIFIED) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SchemeObject* s_make_skylight(SchemeObject* s_radius, SchemeObject* s_num, SchemeObject* s_power) {
    double radius = safe_scm2double(s_radius,1,"make-skylight");
    int num = safe_scm2int(s_num,2,"make-skylight");
    if (RendererSettings::uniqueInstance()->fast_preview) {
        num = 1;
    }
    Skylight* light = new Skylight(radius,num);
    if (s_power != S_UNSPECIFIED) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

void LightsourceFactory::register_procs(Scheme* scheme) {
    scheme->assign("lightsource?",1,0,0,(SchemeObject* (*)()) s_lightsource_p);
    scheme->assign("make-pointlight",1,1,0,(SchemeObject* (*)()) s_make_pointlight);
    scheme->assign("make-arealight",5,1,0,(SchemeObject* (*)()) s_make_arealight);
    scheme->assign("make-spotlight",4,1,0,(SchemeObject* (*)()) s_make_spotlight);
    scheme->assign("make-skylight",2,1,0,(SchemeObject* (*)()) s_make_skylight);
}
