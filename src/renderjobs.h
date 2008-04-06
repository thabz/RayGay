
#ifndef RENDERJOB_POOL
#define RENDERJOB_POOL

#include <pthread.h>
#include <deque>
#include "image/rgba.h"
class Image;

/**
 * The input for the Renderer::render() method
 * which basically defines a subset of the total
 * image to be rendered.
 */
class RenderJob {
    public:
	enum JobType {
	    NEED_PREVIEW,
	    NEED_FULL_RENDER,
	    IS_DONE
	};
	RenderJob() {};
	RenderJob(int b_x, int e_x, int b_y, int e_y) {
	    begin_x = b_x;
	    end_x = e_x;
	    begin_y = b_y;
	    end_y = e_y;
	}
	int begin_x;
	int begin_y;
	int end_x;
	int end_y;
	/// The state of the job
	JobType type;
	/// Upperleft, upperright, lowerleft, lowerright colors of block
	mutable RGBA ul,ur,ll,lr;
	/// The most important jobs will be rendered first
	double importance;
	/// The pixelarea that this job spans
	int area() { return (end_x - begin_x) * (end_y - begin_y); };
};

/**
 * The pool of renderjobs
 */

class RenderJobPool {

    public:
	/// Constructor
	RenderJobPool(int width, int height, int initial_cell_size);
	/// Add a job to the pool.
	void addJob(RenderJob job);
	/// Get next job to be done.
	RenderJob* getJob();
	/// Marks a job done
	void markJobDone(RenderJob* job);

    private:
	void init(int w, int h, int initial_cell_size);
	std::deque<RenderJob*> jobs;
	uint64_t pixels_fully_rendered;
	uint64_t total_image_pixels;
	pthread_mutex_t mutex_jobs;
	pthread_mutex_t mutex_cout;
	int last_percentage;

};

#endif
