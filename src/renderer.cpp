
#include <cassert>
#include <iostream>

#include "cameras/camera.h"
#include "exception.h"
#include "image/image.h"
#include "image/rgb.h"
#include "image/rgba.h"
#include "materials/material.h"
#include "math/halton.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "objects/object.h"
#include "renderer.h"
#include "scene.h"
#include "space/kdtree.h"

#include "samplers/non_aa_sampler.h"
#include "samplers/uniform_jitter.h"
#include "samplers/whitted_adaptive.h"

Renderer::Renderer(RendererSettings *settings, Image *img, Scene *scene,
                   KdTree *spc, RenderJobPool *job_pool, uint32_t thread_id) {
  this->scene = scene;
  this->space = spc;
  this->renderersettings = settings;
  this->job_pool = job_pool;
  this->thread_id = thread_id;
  this->img = img;
  this->aborting = false;
  this->gloss_sequence = new Halton(2, 2);

  Camera *camera = scene->getCamera();
  sampler = camera->getSamplerFactory()->createInstance(img, this);
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
  RenderJob *job;
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
void Renderer::render(const RenderJob &job) {
  scene->getCamera()->resetQMC();
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
void Renderer::renderPreview(const RenderJob &job) {

  int img_w = img->getWidth();
  int img_h = img->getHeight();

  // Find the corner colors
  job.ul = getPixel(
      Vector2(double(job.begin_x) / img_w, double(job.begin_y) / img_h));
  job.ur =
      getPixel(Vector2(double(job.end_x) / img_w, double(job.begin_y) / img_h));
  job.ll =
      getPixel(Vector2(double(job.begin_x) / img_w, double(job.end_y) / img_h));
  job.lr =
      getPixel(Vector2(double(job.end_x) / img_w, double(job.end_y) / img_h));

  // Plot the gradient
  int w = job.end_x - job.begin_x;
  int h = job.end_y - job.begin_y;
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      double dx = double(x) / double(w);
      double dy = double(y) / double(h);
      RGBA u = dx * job.ur + (1 - dx) * job.ul;
      RGBA l = dx * job.lr + (1 - dx) * job.ll;
      RGBA c = dy * l + (1 - dy) * u;

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
 * \f[ R(N,V) = R_0 + \left(1-R_0\right)\left(1-\left(N \cdot V\right)\right)^5
 * \f]
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
Vector2 Renderer::fresnel(Vector normal, const Vector &ray_dir,
                          const Material *material) const {
  double reflectance, reflection, transmission, eta;

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
    reflection = reflectance + (1 - reflectance) * pow(1 - nv, 5);
    transmission =
        material->getKd() + material->getKs() + material->getKt() - reflection;
  } else {
    reflection = 0;
    transmission = 0;
  }
  return Vector2(reflection, transmission);
}
