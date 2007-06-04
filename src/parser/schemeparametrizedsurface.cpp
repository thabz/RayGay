
#include "parser/schemeparametrizedsurface.h"
#include "parser/converters.h"

SchemeParametrizedSurface::SchemeParametrizedSurface(
        Scheme* scheme,
	SchemeObject* s_proc, 
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
    SchemeObject* s_u = double2scm(u); 
    SchemeObject* s_v = double2scm(v); 
    SchemeObject* s_result = scheme->callProcedure_2(procedure_name, s_u, s_v);
    return scm2vector(s_result, "", 0);
}


