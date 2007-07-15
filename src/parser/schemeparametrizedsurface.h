
#ifndef SCHEME_PARAMETRIZED_SURFACE
#define SCHEME_PARAMETRIZED_SURFACE

#include "objects/parametrizedsurface.h"
#include "scheme/scheme.h"

class SchemeParametrizedSurface : public ParametrizedSurface {

    public:
	SchemeParametrizedSurface(Scheme* scheme, SchemeObject* s_proc, uint32_t uRes, uint32_t vRes, bool uClose, bool vClose, Material* material);

    protected:
	Vector eval(double u, double v) const;

    private:
        Scheme* scheme;    
	SchemeObject* procedure_name;
};

#endif
