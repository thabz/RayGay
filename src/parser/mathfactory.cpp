
#include <iostream>

#include "parser/mathfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "materials/perlin.h"

SCM random2(SCM s_min, SCM s_max) 
{
    double min = scm_num2double(s_min,1,"random2");
    double max = scm_num2double(s_max,2,"random2");
    double result = RANDOM(min,max);
    return scm_double2num(result);
}

SCM noise(SCM s_point) 
{
    Vector point = scm2vector(s_point,"noise",1);
    double n = Perlin::noise(point);
    return scm_double2num(n);
}

SCM noise3d(SCM s_point, SCM s_offset) 
{
    Vector point = scm2vector(s_point,"noise3d",1);
    double offset = scm_num2double(s_offset, 2, "noise3d");
    Vector v = Perlin::noise3d(point, offset);
    return vector2scm(v);
}

SCM vdot(SCM s_v1, SCM s_v2) 
{
    Vector v1 = scm2vector(s_v1,"vdot",1);
    Vector v2 = scm2vector(s_v2,"vdot",2);
    double dot = v1 * v2;
    return scm_double2num(dot);
}

SCM vcross(SCM s_v1, SCM s_v2) 
{
    Vector v1 = scm2vector(s_v1,"vcross",1);
    Vector v2 = scm2vector(s_v1,"vcross",2);
    Vector cross = Vector::xProduct(v1,v2);
    return vector2scm(cross);
}

SCM vlength(SCM s_v) 
{
    Vector v = scm2vector(s_v,"vlength",1);
    double length = v.length();
    return scm_double2num(length);
}

void MathFactory::register_procs()
{
    scm_c_define_gsubr("random2",2,0,0, (SCM (*)()) random2);
    scm_c_define_gsubr("noise",1,0,0, (SCM (*)()) noise);
    scm_c_define_gsubr("noise3d",2,0,0, (SCM (*)()) noise3d);

    scm_c_define_gsubr("vdot",2,0,0, (SCM (*)()) vdot);
    scm_c_define_gsubr("vcross",2,0,0, (SCM (*)()) vcross);
    scm_c_define_gsubr("vlength",1,0,0, (SCM (*)()) vlength);
}

