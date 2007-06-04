
#include "parser/schemeisosurface.h"
#include "profiler.h"

pthread_mutex_t SchemeIsosurface::mutex;
bool SchemeIsosurface::mutex_initialized = false;
Profiler* SchemeIsosurface::profiler = NULL;

SchemeIsosurface::SchemeIsosurface(Scheme* scheme, SCM procedure_name, AABox bbox, uint32_t steps, double accuracy, double iso, Material* mat) : IsoSurface(steps, accuracy, iso, mat)
{
    this->bbox = bbox;
    this->procedure_name = procedure_name;
    this->scheme = scheme;
    
    if (this->profiler == NULL) {
        this->profiler = Profiler::create("Scheme iso-callbacks", "Rendering");
    }

    if (!mutex_initialized) {
        pthread_mutex_init(&mutex,NULL);    
        mutex_initialized = true;
    }
}

AABox SchemeIsosurface::_getBoundingBox() const {
    return bbox;
}

double SchemeIsosurface::evaluateFunction(const Vector& point) const {
    pthread_mutex_lock(&mutex);
    SceneObject* procedure = scheme->lookupOrFail(procedure_name);
    if (procedure == NULL) {
        throw scheme_exception("Unbound procedure named " + procedure_name->toString());    
    }
    profiler->start();
    SCM x = s_double2scm(point[0]); 
    SCM y = s_double2scm(point[1]); 
    SCM z = s_double2scm(point[2]); 
    SCM s_result = scheme->callProcedure_3(procedure, x, y, z);
    double result = s_scm2double(s_result, 0, NULL);
    profiler->stop();
    pthread_mutex_unlock(&mutex);
    return result;
}

SceneObject* SchemeIsosurface::clone() const {
    SchemeIsosurface* result = new SchemeIsosurface(*this);
    return result;
}
