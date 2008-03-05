
#include "parser/lightsourcefactory.h"
#include "renderersettings.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "lights/arealight.h"
#include "lights/spotlight.h"
#include "lights/pointlight.h"
#include "lights/skylight.h"

SchemeObject* s_lightsource_p(Scheme* scheme, SchemeObject* object) {
    return isWrappedObjectType(object, LIGHTSOURCE);        
}

SchemeObject* s_make_pointlight(Scheme* scheme, SchemeObject* s_pos, SchemeObject* s_power) {
    Vector pos = scm2vector(s_pos, L"make-pointlight", 1);
    Pointlight* light = new Pointlight(pos);
    if (s_power != S_UNSPECIFIED) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SchemeObject* s_make_arealight(Scheme* scheme, SchemeObject* s_pos, SchemeObject* s_dir, SchemeObject* s_radius, SchemeObject* s_num, SchemeObject* s_jitter, SchemeObject* s_power) {
    
    Vector pos = scm2vector(s_pos, L"make-arealight", 1);
    Vector dir = scm2vector(s_dir, L"make-arealight", 2);
    double radius = safe_scm2double(s_radius,3,L"make-arealight");
    double jitter = safe_scm2double(s_jitter,5,L"make-arealight");
    int num = safe_scm2int(s_num,4,L"make-arealight");
    if (RendererSettings::uniqueInstance()->fast_preview) {
        return s_make_pointlight(scheme, s_pos, s_power);
    }
    Arealight* light = new Arealight(pos,dir,radius,num,jitter);
    if (s_power != S_UNSPECIFIED) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SchemeObject* s_make_spotlight(Scheme* scheme, SchemeObject* s_pos, SchemeObject* s_lookat, SchemeObject* s_angle, SchemeObject* s_cut_angle, SchemeObject* s_power) {
    Vector pos = scm2vector(s_pos, L"make-spotlight", 1);
    Vector lookat= scm2vector(s_lookat, L"make-spotlight", 2);
    double angle = safe_scm2double(s_angle,3,L"make-spotlight");
    double cut_angle = safe_scm2double(s_cut_angle,4,L"make-spotlight");
    Spotlight* light = new Spotlight(pos,lookat,angle,cut_angle);
    if (s_power != S_UNSPECIFIED) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SchemeObject* s_make_skylight(Scheme* scheme, SchemeObject* s_radius, SchemeObject* s_num, SchemeObject* s_power) {
    double radius = safe_scm2double(s_radius,1,L"make-skylight");
    int num = safe_scm2int(s_num,2,L"make-skylight");
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
    scheme->assign(L"lightsource?",1,0,0,(SchemeObject* (*)()) s_lightsource_p);
    scheme->assign(L"make-pointlight",1,1,0,(SchemeObject* (*)()) s_make_pointlight);
    scheme->assign(L"make-arealight",5,1,0,(SchemeObject* (*)()) s_make_arealight);
    scheme->assign(L"make-spotlight",4,1,0,(SchemeObject* (*)()) s_make_spotlight);
    scheme->assign(L"make-skylight",2,1,0,(SchemeObject* (*)()) s_make_skylight);
}
