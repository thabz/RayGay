
#ifndef PARSER_SCHEME_NORMAL_PERTURBER_H
#define PARSER_SCHEME_NORMAL_PERTURBER_H

#include <libguile.h>
#include "materials/normalperturbers/normalperturber.h"

class SchemeNormalPerturber : public NormalPerturber 
{

    public:
	SchemeNormalPerturber(SCM procedure);
	~SchemeNormalPerturber();
	
    protected:
	Vector _perturb(const Vector& point, const Vector& normal) const;

    private:
	SCM s_procedure;
    	static pthread_mutex_t mutex;
    	static bool mutex_initialized;
};

#endif
