
#include <iostream>
#include <libguile.h>
#include <cmath>
#include "parser/schemefunctions.h"

SCM iso_goursats_surface(SCM s_a, SCM s_b, SCM s_c, SCM s_x, SCM s_y, SCM s_z)
{
    char* proc = "iso-goursats-surface";
    double a = s_scm2double(s_a, 1, proc);
    double b = s_scm2double(s_b, 2, proc);
    double c = s_scm2double(s_c, 3, proc);
    double x = s_scm2double(s_x, 4, proc);
    double y = s_scm2double(s_y, 5, proc);
    double z = s_scm2double(s_z, 6, proc);
    double x2 = x * x;
    double y2 = y * y;
    double z2 = z * z;
    double sumxyz = x2 + y2 + z2;
    double result = 
	x2 * x2 + 
	y2 * y2 + 
	z2 * z2 + 
	a * sumxyz * sumxyz +
	b * sumxyz + 
	c;
    return s_double2scm(result);

}

SCM iso_torus(SCM s_R, SCM s_r, SCM s_x, SCM s_y, SCM s_z)
{
    char* proc = "iso-torus";
    double R = s_scm2double(s_R, 1, proc);
    double r = s_scm2double(s_r, 2, proc);
    double x = s_scm2double(s_x, 3, proc);
    double y = s_scm2double(s_y, 4, proc);
    double z = s_scm2double(s_z, 5, proc);

    double a = (R - sqrt(x*x + y*y));
    double result = a * a + z * z - r * r;
    return s_double2scm(result);
}

void SchemeFunctions::register_procs(Scheme* scheme) {
    scheme->assign("iso-torus",5,0,0,(SCM (*)()) iso_torus);
    scheme->assign("iso-goursats-surface",6,0,0,(SCM (*)()) iso_goursats_surface);
}

