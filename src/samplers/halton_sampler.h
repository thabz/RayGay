
#ifndef SAMPLERS_HALTON_SAMPLER
#define SAMPLERS_HALTON_SAMPLER

#include "samplers/sampler.h"
class Halton;

class HaltonSamplerFactory : public SamplerFactory {
public:
  HaltonSamplerFactory(uint32_t num);
  Sampler *createInstance(Image *img, Renderer *renderer);

private:
  uint32_t samples_num;
};

class HaltonSampler : public Sampler {
  friend Sampler *HaltonSamplerFactory::createInstance(Image *, Renderer *);

public:
  virtual void render(const RenderJob &job);

private:
  HaltonSampler(Image *image, Renderer *renderer, uint32_t samples_num);
  uint32_t samples_num;
  Halton *halton_seq;
};

#endif
