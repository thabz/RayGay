
#include <iostream>
#include <cassert>

#include "renderer.h"
#include "exception.h"
#include "cameras/camera.h"
#include "image/image.h"
#include "scene.h"
#include "image/rgb.h"
#include "image/rgba.h"
#include "objects/object.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "space/kdtree.h"
#include "materials/material.h"
#include "math/halton.h"

#include "samplers/whitted_adaptive.h"
#include "samplers/non_aa_sampler.h"
#include "samplers/uniform_jitter.h"

Renderer::Renderer(RendererSettings* settings, Image* img, Scene* scene, KdTree* spc, RenderJobPool* job_pool, uint32_t thread_id) {
    this->scene = scene;
    this->space = spc;
    this->renderersettings = settings;
    this->job_pool = job_pool;
    this->thread_id = thread_id;
    this->img = img;
    this->aborting = false;
    this->gloss_sequence = new Halton(2,2);

    Camera* camera = scene->getCamera();
    sampler = camera->getSamplerFactory()->createInstance(img,this);
}

Renderer::~Renderer() {
    delete gloss_sequence;
    delete sampler;
}

void Renderer::abort() {
    sampler->abort();
    aborting = true;
}

void Renderer::run() {
    RenderJob* job;
    while ((job = job_pool->getJob()) != NULL && !aborting) {
	render(*job);
	job_pool->markJobDone(job);
    }
}

/**
 * Render a job.
 *
 * @param job The job to render
 */
void Renderer::render(const RenderJob& job) {
    if (job.type == RenderJob::NEED_PREVIEW) {
	renderPreview(job);
    } else if (job.type == RenderJob::NEED_FULL_RENDER) {
	sampler->render(job);
    } else {
	throw_exception("Unknown type of render job.");
    }
}

/**
 * Find the colors of the corner-pixels of the job and 
 * plot a gradient into the target image.
 *
 * @param job The job to render
 */
void Renderer::renderPreview(const RenderJob& job) {

    // Find the corner colors 
    job.ul = getPixel(Vector2(job.begin_x,job.begin_y));
    job.ur = getPixel(Vector2(job.end_x,job.begin_y));
    job.ll = getPixel(Vector2(job.begin_x,job.end_y));
    job.lr = getPixel(Vector2(job.end_x,job.end_y));
    
    // Plot the gradient
    int w = job.end_x - job.begin_x;
    int h = job.end_y - job.begin_y;
    int img_h = img->getHeight();
    for(int y = 0; y < h; y++) {
	for(int x = 0; x < w; x++) {
	    double dx = double(x) / double (w);
	    double dy = double(y) / double (h);
	    RGBA u = dx * job.ur + (1-dx) * job.ul;
	    RGBA l = dx * job.lr + (1-dx) * job.ll;
	    RGBA c = dy * l + (1-dy) * u;

	    int img_x = x + job.begin_x;
	    int img_y = y + job.begin_y;
	    img->setRGBA(img_x, img_h - img_y - 1, c);
	}
    }
    return;
}


/**
 * The Fresnel equation.
 *
 * Schlicks approximation to the Fresnel equation is
 *
 * \f[ R(N,V) = R_0 + \left(1-R_0\right)\left(1-\left(N \cdot V\right)\right)^5 \f]
 *
 * where the reflectance \f$R_0\f$ is
 *
 * \f[ R_0 = \frac{(1-\eta)^2}{(1+\eta)^2} \f]
 *
 * @param normal The surface normal which is \f$N\f$
 * @param ray_dir Ray direction which is \f$V\f$
 * @param material The material of the surface
 *
 * @return (reflection,transmission)
 */
Vector2 Renderer::fresnel(Vector normal, const Vector& ray_dir, const Material* material) const {
    double reflectance,reflection,transmission,eta;

    if (material->getKt() > 0.0) {
	if (normal * ray_dir > 0) {
	    eta = material->getEta();
	    normal *= -1;
	} else {
	    eta = 1.0 / material->getEta();
	}
	reflectance = ((1 - eta) * (1 - eta)) / ((1 + eta) * (1 + eta));
    } else {
	reflectance = material->getKs();
    }

    double nv = -(normal * ray_dir);

    if (nv > EPSILON) {
	reflection = reflectance + (1 - reflectance) * pow(1-nv,5);
	transmission = material->getKd() + material->getKs() + material->getKt() - reflection;
    } else {
	reflection = 0;
	transmission = 0;
    }
    return Vector2(reflection,transmission);

}

