
#ifndef TRACER_SAMPLERS_SAMPLER
#define TRACER_SAMPLERS_SAMPLER

class RenderJob;

#include "image/rgba.h"
#include "image/image.h"
#include "math/vector2.h"
#include "renderer.h"

class SamplerFactory 
{
    public:
	virtual Sampler* createInstance(Image* img, Renderer* renderer) = 0;
	virtual ~SamplerFactory() {};
};

class Sampler 
{
    public:
	virtual void render(const RenderJob& job) = 0;
	virtual ~Sampler();

	void abort();

    protected:
	void setPixel(int x, int y, const RGBA& color);
	RGBA sample(double x, double y);
	RGBA sample(const Vector2& pos);
	Sampler(Image* img, Renderer* renderer);
	bool aborting;

	Image* image;
	Renderer* renderer;
};

inline
void Sampler::setPixel(int x, int y, const RGBA& color)
{
    image->setRGBA(x,image->getHeight() - y - 1,color);
}

inline
RGBA Sampler::sample(double x, double y)
{
    return renderer->getPixel(Vector2(x / image->getWidth(), y / image->getHeight()));
}

inline
RGBA Sampler::sample(const Vector2& pos)
{
    return sample(pos[0],pos[1]);
}


#endif
