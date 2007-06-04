
#ifndef SCHEME_PARAMETRIZED_SURFACE
#define SCHEME_PARAMETRIZED_SURFACE

#include "objects/parametrizedsurface.h"
#include <libguile.h>

class SchemeParametrizedSurface : public ParametrizedSurface {

    public:
	SchemeParametrizedSurface(Scheme* scheme, SCM s_proc, uint uRes, uint vRes, bool uClose, bool vClose, Material* material);

    protected:
	Vector eval(double u, double v) const;

    private:
        Scheme* scheme;    
	SCM procedure_name;
};

#endif
