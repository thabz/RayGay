
#include "parser/schemeisosurface.h"
#include "profiler.h"

pthread_mutex_t SchemeIsosurface::mutex;
bool SchemeIsosurface::mutex_initialized = false;
Profiler* SchemeIsosurface::profiler = NULL;

SchemeIsosurface::SchemeIsosurface(SCM procedure_name, AABox bbox, uint32_t steps, double accuracy, double iso, Material* mat) : IsoSurface(steps, accuracy, iso, mat)
{
    this->bbox = bbox;
    this->procedure_name = procedure_name;

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
    profiler->start();
    SCM x = scm_double2num(point[0]); 
    SCM y = scm_double2num(point[1]); 
    SCM z = scm_double2num(point[2]); 
    SCM s_result = scm_call_3(procedure_name, x, y, z);
    double result = scm_num2double(s_result, 0, NULL);
    profiler->stop();
    pthread_mutex_unlock(&mutex);
    return result;
}

SceneObject* SchemeIsosurface::clone() const {
    SchemeIsosurface* result = new SchemeIsosurface(*this);
    return result;
}
