
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

void MathFactory::register_procs()
{
    scm_c_define_gsubr("random2",2,0,0, (SCM (*)()) random2);
    scm_c_define_gsubr("noise",1,0,0, (SCM (*)()) noise);
    scm_c_define_gsubr("noise3d",2,0,0, (SCM (*)()) noise3d);
}

