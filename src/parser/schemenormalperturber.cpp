
#include "parser/schemenormalperturber.h"
#include "parser/converters.h"

SchemeNormalPerturber::SchemeNormalPerturber(SCM procedure) 
{
    this->procedure_name = procedure;
}

Vector SchemeNormalPerturber::_perturb(const Vector& P, const Vector& N) const 
{
    SCM s_point = vector2scm(P);
    SCM s_normal = vector2scm(N);
    SCM s_result = scm_call_2(procedure_name, s_point, s_normal);
    Vector result = scm2vector(s_result, NULL, 0);
    result.normalize();
    return result;
}

