
#include <iostream>

#include "parser/mathfactory.h"
#include "parser/converters.h"
#include "parser/wrapper.h"

SCM MathFactory::random(SCM s_min, SCM s_max) 
{
    double min = scm_num2double(s_min,0,"random");
    double max = scm_num2double(s_max,1,"random");
    double result = RANDOM(min,max);
    return scm_double2num(result);
}

void MathFactory::register_procs()
{
    scm_c_define_gsubr("random2",2,0,0,
	    (SCM (*)()) MathFactory::random);
}
