
#ifndef RENDERJOB_POOL
#define RENDERJOB_POOL

#include <pthread.h>
#include <vector>
class Image;

/**
 * The input for the Renderer::render() method
 * which basically defines a subset of the total
 * image to be rendered.
 */
class RenderJob {
    public:
	int begin_x;
	int begin_y;
	int end_x;
	int end_y;
};

/**
 * The pool of renderjobs
 */
class RenderJobPool {

    public:
	/// Constructor
	RenderJobPool();
	/// Add a job to the pool.
	void addJob(const RenderJob& job);
	/// Get next job to be done.
	bool getJob(RenderJob* job_dest);

    private:
	std::vector<RenderJob> jobs;
	unsigned int next_job;
	pthread_mutex_t mutex_jobs;
};

#endif
