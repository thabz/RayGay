
#include "samplers/uniform_jitter.h"
#include "renderer.h"
#include "image/image.h"
#include "renderjobs.h"
#include "math/vector2.h"

UniformJitter::UniformJitter(Image* image, Renderer* renderer) : Sampler(image,renderer) 
{

}

void UniformJitter::render(const RenderJob& job) 
{
    RGBA color;
    for (int y = job.begin_y; y < job.end_y && !aborting; y++) {
	for (int x = job.begin_x; x < job.end_x && !aborting; x++) {
	    // TODO: Jitter x,y
	    color = sample(Vector2(x,y));
	    setPixel(x,y,color);
	}
    }
}

Sampler* UniformJitter::clone() 
{
    return new UniformJitter(image,renderer);
}

