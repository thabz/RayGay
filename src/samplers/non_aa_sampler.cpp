
#include "samplers/non_aa_sampler.h"
#include "image/image.h"
#include "math/vector2.h"
#include "renderer.h"
#include "renderjobs.h"

NonAASampler::NonAASampler(Image *image, Renderer *renderer)
    : Sampler(image, renderer) {}

void NonAASampler::render(const RenderJob &job) {
  RGBA color;
  for (int y = job.begin_y; y < job.end_y && !aborting; y++) {
    for (int x = job.begin_x; x < job.end_x && !aborting; x++) {
      // TODO: Jitter x,y
      color = sample(double(x), double(y));
      setPixel(x, y, color);
    }
  }
}

NonAASamplerFactory::NonAASamplerFactory() {}

Sampler *NonAASamplerFactory::createInstance(Image *img, Renderer *renderer) {
  return new NonAASampler(img, renderer);
}
