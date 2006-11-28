
#include <iostream>

#include "parser/mathfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "materials/perlin.h"
#include "math/poisson_disc.h"

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

SCM vcross(SCM s_v1, SCM s_v2) 
{
    Vector v1 = scm2vector(s_v1,"vcross",1);
    Vector v2 = scm2vector(s_v2,"vcross",2);
    Vector cross = Vector::xProduct(v1,v2);
    return vector2scm(cross);
}

SCM vrandomunit() {
    Vector v = Vector::randomUnitVector();
    return vector2scm(v);
}

SCM make_poisson_set(SCM s_w, SCM s_h, SCM s_r,  SCM s_num) {
    char* proc = "make-poisson-disc-set";
    double w = scm_num2double(s_w, 1, proc);
    double h = scm_num2double(s_h, 2, proc);
    double r = scm_num2double(s_r, 3, proc);
    int num = scm_num2int(s_num, 4, proc);
    Vector2* set = new Vector2[num];
    int real_num = PoissonDiscDistribution::createSet(w,h,r,num,set);
    SCM s_set = SCM_EOL;
    for(int i = 0; i < real_num; i++) {
	double x = set[i][0];
	double y = set[i][1];
	SCM s_point = scm_list_2(scm_double2num(x), scm_double2num(y));
	SCM s_point_wrap = scm_list_1(s_point);
	s_set = scm_append(scm_list_2(s_set,s_point_wrap));
    }
    return s_set;
}

void MathFactory::register_procs()
{
    scm_c_define_gsubr("random2",2,0,0, (SCM (*)()) random2);
    scm_c_define_gsubr("noise",1,0,0, (SCM (*)()) noise);
    scm_c_define_gsubr("noise3d",2,0,0, (SCM (*)()) noise3d);
    scm_c_define_gsubr("vcross",2,0,0, (SCM (*)()) vcross);
    scm_c_define_gsubr("vrandomunit",0,0,0, (SCM (*)()) vrandomunit);
    scm_c_define_gsubr("make-poisson-disc-set",4,0,0, (SCM (*)()) make_poisson_set);
}

