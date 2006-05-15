
#include "samplers/uniform_jitter.h"
#include "renderer.h"
#include "image/image.h"
#include "renderjobs.h"
#include "math/vector2.h"

UniformJitter::UniformJitter(Image* image, Renderer* renderer, uint32_t samples_sqrt) : Sampler(image,renderer) 
{
    assert(samples_sqrt > 0);
    this->samples_sqrt = samples_sqrt;
}

void UniformJitter::render(const RenderJob& job) 
{
    RGBA color;
    double x_r, y_r;
    double jitter_width = 1.0 / double(samples_sqrt);
    for (int y = job.begin_y; y < job.end_y && !aborting; y++) {
	for (int x = job.begin_x; x < job.end_x && !aborting; x++) {
	    color = RGBA(0.0,0.0,0.0,0.0);
	    for(int x_0 = 0; x_0 < samples_sqrt; x_0++) {
		for(int y_0 = 0; y_0 < samples_sqrt; y_0++) {
		    x_r = double(x) + (double(x_0) / double(samples_sqrt));
		    y_r = double(y) + (double(y_0) / double(samples_sqrt));
		    x_r += RANDOM(0.0, jitter_width);
		    y_r += RANDOM(0.0, jitter_width);
		    color += sample(Vector2(x_r,y_r));
		}
	    }
	    color = color / (samples_sqrt * samples_sqrt);
	    setPixel(x,y,color);
	}
    }
}

UniformJitterFactory::UniformJitterFactory(uint32_t samples_sqrt)
{
    this->samples_sqrt = samples_sqrt;
}

Sampler* UniformJitterFactory::createInstance(Image* img, Renderer* renderer)
{
    return new UniformJitter(img, renderer, samples_sqrt);
}
