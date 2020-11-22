
#include "samplers/sampler.h"
#include "renderjobs.h"

Sampler::Sampler(Image *img, Renderer *renderer) {
  this->image = img;
  this->renderer = renderer;
  this->aborting = false;
}

Sampler::~Sampler() {}

void Sampler::abort() { this->aborting = true; }
