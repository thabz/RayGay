
#ifndef SAMPLERS_UNIFORM_JITTER
#define SAMPLERS_UNIFORM_JITTER

#include "samplers/sampler.h"

class UniformJitterFactory: public SamplerFactory
{
    public:
	UniformJitterFactory(uint32_t samples);
	Sampler* createInstance(Image* img, Renderer* renderer);
	
    private:
	uint32_t samples_sqrt;
};

class UniformJitter : public Sampler 
{
    friend Sampler* UniformJitterFactory::createInstance(Image*, Renderer*);

    public:
	virtual void render(const RenderJob& job);
	
    private:
	UniformJitter(Image* image, Renderer* renderer, uint32_t samples_sqrt);
	uint32_t samples_sqrt;

};

#endif

