
#include "parser/schemenormalperturber.h"
#include "parser/converters.h"
#include "profiler.h"

pthread_mutex_t SchemeNormalPerturber::mutex;
bool SchemeNormalPerturber::mutex_initialized = false;

SchemeNormalPerturber::SchemeNormalPerturber(SCM procedure) 
{
    this->s_procedure = procedure;
    this->profiler = Profiler::create("Scheme callbacks", "Rendering");
    
    if (!mutex_initialized) {
        pthread_mutex_init(&mutex,NULL);    
        mutex_initialized = true;
    }
}

SchemeNormalPerturber::~SchemeNormalPerturber() {
    if (mutex_initialized) {
        pthread_mutex_destroy(&mutex);
        mutex_initialized = false;    
    }        
}

Vector SchemeNormalPerturber::_perturb(const Vector& P, const Vector& N) const 
{
    pthread_mutex_lock(&mutex);
    profiler->start();
    SCM s_point = vector2scm(P);
    SCM s_normal = vector2scm(N);
    SCM s_result = scm_call_2(s_procedure, s_point, s_normal);
    Vector result = scm2vector(s_result, NULL, 0);
    profiler->stop();
    pthread_mutex_unlock(&mutex);
    result.normalize();
    return result;
}

