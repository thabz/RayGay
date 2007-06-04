
#include "parser/schemeparametrizedsurface.h"
#include "parser/converters.h"

SchemeParametrizedSurface::SchemeParametrizedSurface(
        Scheme* scheme,
	SCM s_proc, 
	uint32_t uRes, 
	uint32_t vRes, 
	bool uClose, 
	bool vClose, 
	Material* material) : 
    ParametrizedSurface(uRes, vRes, uClose, vClose, material)
{
    this->procedure_name = s_proc;
    this->scheme = scheme;
}

Vector SchemeParametrizedSurface::eval(double u, double v) const
{
    SCM s_u = s_double2scm(u); 
    SCM s_v = s_double2scm(v); 
    SCM s_result = scheme->callProcedure_2(procedure_name, s_u, s_v);
    return scm2vector(s_result, "", 0);
}


