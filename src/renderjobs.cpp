
#include "renderjobs.h"

RenderJobPool::RenderJobPool() {
    next_job = 0;
}

bool RenderJobPool::getJob(RenderJob* job_dest) {
    bool result;

    pthread_mutex_lock(&mutex_jobs);
    if (next_job == jobs.size()) {
	result = false;
    } else {
	(*job_dest) = jobs[next_job];
	result = true;
	next_job++;
    }
    pthread_mutex_unlock(&mutex_jobs);
    return result;
}

void RenderJobPool::addJob(const RenderJob& job) {
    jobs.push_back(job);
}
