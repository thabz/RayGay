#ifndef RENDERER_H
#define RENDERER_H

#include "scene.h"

class RGB;
class Image;

/**
 * An interface all renderers must implement.
 */
class Renderer {

    public:
	void render(Scene* scene, Image*);

    private:
	virtual RGB getPixel(double x, double y) = 0;

    protected:
	Renderer();
	Scene* scene;
};

#endif
