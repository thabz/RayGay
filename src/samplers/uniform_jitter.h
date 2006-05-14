
#ifndef SAMPLERS_UNIFORM_JITTER
#define SAMPLERS_UNIFORM_JITTER

#include "samplers/sampler.h"

class UniformJitter : public Sampler 
{
    public:
	UniformJitter(Image* image, Renderer* renderer);

	virtual void render(const RenderJob& job);

};

#endif

