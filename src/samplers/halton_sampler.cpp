
#include <cassert>
#include "samplers/halton_sampler.h"
#include "renderer.h"
#include "image/image.h"
#include "renderjobs.h"
#include "math/vector2.h"
#include "math/halton.h"

HaltonSampler::HaltonSampler(Image* image, Renderer* renderer, uint32_t samples_num) : Sampler(image,renderer) 
{
    assert(samples_num > 0);
    this->samples_num = samples_num;
    this->halton_seq = new Halton(2,2);
}

void HaltonSampler::render(const RenderJob& job) 
{
    RGBA color;
    halton_seq->reset();
    RGBA c[samples_num];
    for (int y = job.begin_y; y < job.end_y && !aborting; y++) {
	for (int x = job.begin_x; x < job.end_x && !aborting; x++) {
	    for(uint32_t n = 0; n < samples_num; n++) {
		double* p = halton_seq->getNext();
		c[n] = sample(p[0]+x,p[1]+y);
	    }
	    color = RGBA::avg(c,samples_num);
	    setPixel(x,y,color);
	}
    }
}

HaltonSamplerFactory::HaltonSamplerFactory(uint32_t samples_num)
{
    this->samples_num = samples_num;
}

Sampler* HaltonSamplerFactory::createInstance(Image* img, Renderer* renderer)
{
    return new HaltonSampler(img, renderer, samples_num);
}
