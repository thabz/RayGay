
#include "parser/lightsourcefactory.h"
#include "renderersettings.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "lights/arealight.h"
#include "lights/spotlight.h"
#include "lights/pointlight.h"
#include "lights/skylight.h"

SCM make_pointlight(SCM s_pos, SCM s_power) {
    Vector pos = scm2vector(s_pos, "make-pointlight", 1);
    Pointlight* light = new Pointlight(pos);
    if (!(SCM_UNBNDP (s_power) || SCM_FALSEP (s_power))) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SCM make_arealight(SCM s_pos, SCM s_dir, SCM s_radius, SCM s_num, SCM s_jitter, SCM s_power) {
    
    Vector pos = scm2vector(s_pos, "make-arealight", 1);
    Vector dir = scm2vector(s_dir, "make-arealight", 2);
    double radius = scm_num2double(s_radius,3,"make-arealight");
    double jitter = scm_num2double(s_jitter,5,"make-arealight");
    int num = scm_num2int(s_num,4,"make-arealight");
    if (RendererSettings::uniqueInstance()->fast_preview) {
        num = 1;
    }
    Arealight* light = new Arealight(pos,dir,radius,num,jitter);
    if (!(SCM_UNBNDP (s_power) || SCM_FALSEP (s_power))) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SCM make_spotlight(SCM s_pos, SCM s_lookat, SCM s_angle, SCM s_cut_angle, SCM s_power) {
    Vector pos = scm2vector(s_pos, "make-spotlight", 1);
    Vector lookat= scm2vector(s_lookat, "make-spotlight", 2);
    double angle = scm_num2double(s_angle,3,"make-spotlight");
    double cut_angle = scm_num2double(s_cut_angle,4,"make-spotlight");
    Spotlight* light = new Spotlight(pos,lookat,angle,cut_angle);
    if (!(SCM_UNBNDP (s_power) || SCM_FALSEP (s_power))) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

SCM make_skylight(SCM s_radius, SCM s_num, SCM s_power) {
    double radius = scm_num2double(s_radius,1,"make-skylight");
    int num = scm_num2int(s_num,2,"make-skylight");
    if (RendererSettings::uniqueInstance()->fast_preview) {
        num = 1;
    }
    Skylight* light = new Skylight(radius,num);
    if (!(SCM_UNBNDP (s_power) || SCM_FALSEP (s_power))) {
	RGB power = scm2rgb(s_power);
	light->setPower(power);
    }
    return lightsource2scm(light);
}

void LightsourceFactory::register_procs() {
    scm_c_define_gsubr("make-pointlight",1,1,0,(SCM (*)()) make_pointlight);
    scm_c_define_gsubr("make-arealight",5,1,0,(SCM (*)()) make_arealight);
    scm_c_define_gsubr("make-spotlight",4,1,0,(SCM (*)()) make_spotlight);
    scm_c_define_gsubr("make-skylight",2,1,0,(SCM (*)()) make_skylight);
}
