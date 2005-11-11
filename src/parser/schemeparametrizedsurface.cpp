
#include "parser/schemeparametrizedsurface.h"
#include "parser/converters.h"

SchemeParametrizedSurface::SchemeParametrizedSurface(
	SCM s_proc, 
	uint32_t uRes, 
	uint32_t vRes, 
	bool uClose, 
	bool vClose, 
	Material* material) : 
    ParametrizedSurface(uRes, vRes, uClose, vClose, material)
{
    this->procedure_name = s_proc;
}

Vector SchemeParametrizedSurface::eval(double u, double v) const
{
    SCM s_u = scm_double2num(u); 
    SCM s_v = scm_double2num(v); 
    SCM s_result = scm_call_2(procedure_name, s_u, s_v);
    return scm2vector(s_result, "", 0);
}


