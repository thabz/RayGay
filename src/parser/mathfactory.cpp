
#include <iostream>

#include "parser/mathfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"
#include "materials/perlin.h"
#include "math/poisson_disc.h"
#include "math/halton.h"

SCM random2(SCM s_min, SCM s_max) 
{
    double min = s_scm2double(s_min,1,"random2");
    double max = s_scm2double(s_max,2,"random2");
    double result = RANDOM(min,max);
    return s_double2scm(result);
}

SCM noise(SCM s_point) 
{
    Vector point = scm2vector(s_point,"noise",1);
    double n = Perlin::noise(point);
    return s_double2scm(n);
}

SCM noise3d(SCM s_point, SCM s_offset) 
{
    Vector point = scm2vector(s_point,"noise3d",1);
    double offset = s_scm2double(s_offset, 2, "noise3d");
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

SCM vdistance(SCM s_v1, SCM s_v2) 
{
    Vector v1 = scm2vector(s_v1,"vdistance",1);
    Vector v2 = scm2vector(s_v2,"vdistance",2);
    double d = (v1-v2).length();
    return s_double2scm(d);
}

SCM vrandomunit() {
    Vector v = Vector::randomUnitVector();
    return vector2scm(v);
}

SCM make_poisson_set(SCM s_w, SCM s_h, SCM s_r,  SCM s_num) {
    char* proc = "make-poisson-disc-set";
    double w = s_scm2double(s_w, 1, proc);
    double h = s_scm2double(s_h, 2, proc);
    double r = s_scm2double(s_r, 3, proc);
    int num = scm2int(s_num, 4, proc);
    Vector2* set = new Vector2[num];
    PoissonDiscDistribution distr = PoissonDiscDistribution(w,h);
    int real_num = distr.createSet(r,num,set);
    SCM s_set = S_EMPTY_LIST;
    for(int i = 0; i < real_num; i++) {
	double x = set[i][0];
	double y = set[i][1];
	SCM s_point = i_list_2(s_double2scm(x), s_double2scm(y));
	SCM s_point_wrap = i_list_1(s_point);
	s_set = s_append(i_list_2(s_set,s_point_wrap)); // TODO: Use cons and not append
    }
    return s_set;
}

SCM make_halton_set(SCM s_w, SCM s_h, SCM s_num) {
    char* proc = "make-halton-set";
    double w = s_scm2double(s_w, 1, proc);
    double h = s_scm2double(s_h, 2, proc);
    int num = scm2int(s_num, 3, proc);
    Halton halton = Halton(2,2);
    SCM s_set = S_EMPTY_LIST;
    for(int i = 0; i < num; i++) {
        double *values = halton.getNext();
	SCM s_point = i_list_2(s_double2scm(w*values[0]), s_double2scm(h*values[1]));
	SCM s_point_wrap = i_list_1(s_point);
	s_set = s_append(i_list_2(s_set,s_point_wrap));  // TODO: Use cons and not append
    }
    return s_set;
}


void MathFactory::register_procs(Scheme* scheme)
{
    scheme->assign("random2",2,0,0, (SCM (*)()) random2);
    scheme->assign("noise",1,0,0, (SCM (*)()) noise);
    scheme->assign("noise3d",2,0,0, (SCM (*)()) noise3d);
    scheme->assign("vcross",2,0,0, (SCM (*)()) vcross);
    scheme->assign("vdistance",2,0,0, (SCM (*)()) vdistance);
    scheme->assign("vrandomunit",0,0,0, (SCM (*)()) vrandomunit);
    scheme->assign("make-poisson-disc-set",4,0,0, (SCM (*)()) make_poisson_set);
    scheme->assign("make-halton-set",3,0,0, (SCM (*)()) make_halton_set);
}

