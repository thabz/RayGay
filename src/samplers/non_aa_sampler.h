
#ifndef SAMPLERS_NON_AA_SAMPLER
#define SAMPLERS_NON_AA_SAMPLER

#include "samplers/sampler.h"

class NonAASampler : public Sampler 
{
    public:
	NonAASampler(Image* image, Renderer* renderer);
	void render(const RenderJob& job);
	Sampler* clone();
};

class NonAASamplerFactory: public SamplerFactory
{
    public:
	NonAASamplerFactory();
	Sampler* createInstance(Image* img, Renderer* renderer);
};

#endif
