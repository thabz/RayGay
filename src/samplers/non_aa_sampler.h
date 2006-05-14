
#ifndef SAMPLERS_NON_AA_SAMPLER
#define SAMPLERS_NON_AA_SAMPLER

#include "samplers/sampler.h"

class NonAASampler : public Sampler 
{
    public:
	NonAASampler(Image* image, Renderer* renderer);
	void render(const RenderJob& job);
};

#endif
