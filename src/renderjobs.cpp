#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "renderjobs.h"
#include <iostream>
#include "environment.h"
#include "window.h"

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
#ifdef HAVE_GTK
	double progress = double(next_job) / double (jobs.size());
	Environment::getUniqueInstance()->getPreviewWindow()->setProgress(progress);
#endif	
    } else {
	result = false;
    }
    pthread_mutex_unlock(&mutex_jobs);
    return result;
}

void RenderJobPool::addJob(RenderJob job) {
    job.is_done = false;
    jobs.push_back(job);
}

void RenderJobPool::markJobDone(RenderJob* job) {
    job->is_done = true;
#ifdef HAVE_GTK
    // Update preview window
    pthread_mutex_lock(&mutex_jobs);
    int x = job->begin_x;
    int y = job->begin_y;
    int w = job->end_x - x;
    int h = job->end_y - y;
    Environment::getUniqueInstance()->getPreviewWindow()->drawBlock(x,y,w,h);
    pthread_mutex_unlock(&mutex_jobs);
#endif    
}
