
#include "renderjobs.h"
#include <iostream>

RenderJobPool::RenderJobPool() {
    pthread_mutex_init(&mutex_jobs,NULL);
    next_job = 0;
}

bool RenderJobPool::getJob(RenderJob* job_dest) {
    bool result;

    pthread_mutex_lock(&mutex_jobs);
    if (next_job < jobs.size()) {
	(*job_dest) = jobs[next_job];
	next_job++;
	result = true;
	std::cout << next_job << " / " << jobs.size() << "          \r" << std::flush;
    } else {
	result = false;
    }
    pthread_mutex_unlock(&mutex_jobs);
    return result;
}

void RenderJobPool::addJob(const RenderJob& job) {
    jobs.push_back(job);
}
