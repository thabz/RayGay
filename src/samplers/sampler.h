
#ifndef TRACER_SAMPLERS_SAMPLER
#define TRACER_SAMPLERS_SAMPLER

class RenderJob;

#include "image/rgba.h"
#include "image/image.h"
#include "math/vector2.h"
#include "renderer.h"

class Sampler 
{
    public:
	Sampler(Image* img, Renderer* renderer);

	virtual void render(const RenderJob& job) = 0;
	virtual ~Sampler();

	void abort();
	virtual Sampler* clone() = 0;

    protected:
	void setPixel(int x, int y, const RGBA& color);
	RGBA sample(double x, double y);
	RGBA sample(const Vector2& pos);
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
    return renderer->getPixel(Vector2(x,y));
}

inline
RGBA Sampler::sample(const Vector2& pos)
{
    return renderer->getPixel(pos);
}


#endif
