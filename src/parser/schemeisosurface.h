
#ifndef SCHEME_ISOSURFACE_H
#define SCHEME_ISOSURFACE_H

#include "objects/isosurface.h"
#include "scheme/scheme.h"

class Profiler;

class SchemeIsosurface : public IsoSurface {
    public:
	SchemeIsosurface(Scheme* scheme, SchemeObject* procedure_name, AABox bbox, uint32_t steps, double accuracy, double iso, Material* mat);
	SceneObject* clone() const;

    protected:
	AABox _getBoundingBox() const;
	double evaluateFunction(const Vector& point) const;

    private:
        Scheme* scheme;    
	SchemeObject* procedure_name;
	AABox bbox;
	static Profiler* profiler;
    	static pthread_mutex_t mutex;
    	static bool mutex_initialized;
};

#endif
