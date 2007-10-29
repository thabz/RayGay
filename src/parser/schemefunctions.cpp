
#include <cmath>

#include "parser/schemefunctions.h"
#include "parser/converters.h"

SchemeObject* iso_goursats_surface(SchemeObject* s_a, SchemeObject* s_b, SchemeObject* s_c, SchemeObject* s_x, SchemeObject* s_y, SchemeObject* s_z)
{
    wchar_t* proc = L"iso-goursats-surface";
    double a = safe_scm2double(s_a, 1, proc);
    double b = safe_scm2double(s_b, 2, proc);
    double c = safe_scm2double(s_c, 3, proc);
    double x = safe_scm2double(s_x, 4, proc);
    double y = safe_scm2double(s_y, 5, proc);
    double z = safe_scm2double(s_z, 6, proc);
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
    return double2scm(result);

}

SchemeObject* iso_torus(SchemeObject* s_R, SchemeObject* s_r, SchemeObject* s_x, SchemeObject* s_y, SchemeObject* s_z)
{
    wchar_t* proc = L"iso-torus";
    double R = safe_scm2double(s_R, 1, proc);
    double r = safe_scm2double(s_r, 2, proc);
    double x = safe_scm2double(s_x, 3, proc);
    double y = safe_scm2double(s_y, 4, proc);
    double z = safe_scm2double(s_z, 5, proc);

    double a = (R - sqrt(x*x + y*y));
    double result = a * a + z * z - r * r;
    return double2scm(result);
}

void SchemeFunctions::register_procs(Scheme* scheme) {
    scheme->assign(L"iso-torus",5,0,0,(SchemeObject* (*)()) iso_torus);
    scheme->assign(L"iso-goursats-surface",6,0,0,(SchemeObject* (*)()) iso_goursats_surface);
}

