#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "renderjobs.h"
#include <iostream>
#include <algorithm>
#include "environment.h"
#include "window.h"
#include "image/image.h"

/**
 * Construct a new job pool and create inital tiles/jobs.
 * 
 * @param w Width of the target image
 * @param h Height of the target image
 * @param initial_cell_size Size of the initial tiles
 */
RenderJobPool::RenderJobPool(int w, int h, int initial_cell_size) {
    pthread_mutex_init(&mutex_jobs,NULL);
    pixels_fully_rendered = 0;
    total_image_pixels = w * h;
    init(w,h,initial_cell_size);
}

void RenderJobPool::init(int w, int h, int cell_size) {
    RenderJob job;
    double count = 0;
    for(int y = 0; y < (h / cell_size)+1; y++) {
	job.begin_y = y*cell_size;
	job.end_y = min((y+1)*cell_size,h);
	for(int x = 0; x < (w / cell_size)+1; x++) {
	    job.begin_x = x*cell_size;
	    job.end_x = min((x+1)*cell_size, w);
	    if (job.begin_x < w &&
		job.begin_y < h &&
		job.begin_x < job.end_x && 
		job.begin_y < job.end_y) {
		job.importance = 1000000 + cell_size*cell_size + (count++);
		job.type = RenderJob::NEED_PREVIEW;
		addJob(job);
	    }
	}
    }

}

// Sort jobs by descending importance
class compageJobsDesc {
    public:
	bool operator()(const RenderJob* p1, const RenderJob* p2) {
	    return p1->importance > p2->importance;
	}
};

RenderJob* RenderJobPool::getJob() {

    pthread_mutex_lock(&mutex_jobs);

    if (jobs.size() == 0) {
	return NULL;
    }

    std::sort(jobs.begin(),jobs.end(),compageJobsDesc());
    RenderJob *result = jobs.front();
    jobs.pop_front();
    
    pthread_mutex_unlock(&mutex_jobs);
    return result;
}

void RenderJobPool::addJob(RenderJob job) {
    jobs.push_back(new RenderJob(job));
}

void RenderJobPool::markJobDone(RenderJob* job) {
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
    // Find out whether this job need to be split
    if (job->type == RenderJob::NEED_PREVIEW) {
	RGBA mean = (job->ul + job->ur + job->ll + job->lr) * 0.25;
	double variance = mean.sqrDistance(job->ul) +
	    mean.sqrDistance(job->ur) +
	    mean.sqrDistance(job->ll) +
	    mean.sqrDistance(job->lr);
	if (variance < 0.01) {
	    job->importance = variance;
	    job->type = RenderJob::NEED_FULL_RENDER;
	    jobs.push_back(job);
	} else if (job->area()<= 32*32) {
	    job->importance = variance * job->area();
	    job->type = RenderJob::NEED_FULL_RENDER;
	    jobs.push_back(job);
	} else {
	    double new_importance = 1000 + (variance * job->area()) / 4;
	    int center_x = (job->end_x + job->begin_x) / 2;
	    int center_y = (job->end_y + job->begin_y) / 2;
	    RenderJob* job1 = new RenderJob(job->begin_x,center_x,job->begin_y,center_y);
	    RenderJob* job2 = new RenderJob(center_x,job->end_x,job->begin_y,center_y);
	    RenderJob* job3 = new RenderJob(job->begin_x,center_x,center_y,job->end_y);
	    RenderJob* job4 = new RenderJob(center_x,job->end_x,center_y,job->end_y);
	    job1->importance = new_importance;
	    job2->importance = new_importance;
	    job3->importance = new_importance;
	    job4->importance = new_importance;
	    job1->type = RenderJob::NEED_PREVIEW;
	    job2->type = RenderJob::NEED_PREVIEW;
	    job3->type = RenderJob::NEED_PREVIEW;
	    job4->type = RenderJob::NEED_PREVIEW;
	    jobs.push_back(job1);
	    jobs.push_back(job2);
	    jobs.push_back(job3);
	    jobs.push_back(job4);
	}
    } else if (job->type == RenderJob::NEED_FULL_RENDER) {
	pixels_fully_rendered += job->area();
	job->type = RenderJob::IS_DONE;
        #ifdef HAVE_GTK
	double progress = double(pixels_fully_rendered) / double(total_image_pixels);
	Environment::getUniqueInstance()->getPreviewWindow()->setProgress(progress);
        #endif	
    }
}
