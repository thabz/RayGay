#ifndef RENDERER_H
#define RENDERER_H

#include "scene.h"

class RGB;
class Image;

/**
 * An abstract class all renderers must implement.
 */
class Renderer {

    public:
	/// Render a scene into an image
	void render(Scene* scene, Image*, SpaceSubdivider* space);

    private:
	/// The public render-method uses this to render the image. Subclasses must implement this.
	virtual RGB getPixel(double x, double y) = 0;

    protected:
	Renderer();
	/// The scene to be rendered can be accessed from implementations of Renderer.
	Scene* scene;
	SpaceSubdivider* space;
};

#endif
