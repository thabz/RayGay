
#include <iostream>

#include "renderer.h"
#include "image.h"
#include "scene.h"
#include "rgb.h"
#include "object.h"
#include "spacesubdivider.h"

Renderer::Renderer() {
}

void Renderer::render(Scene* sc, Image* img, SpaceSubdivider* spc) {
    scene = sc;
    space = spc;

    // Add all objects in scene to spacesubdivider
    std::vector<object*> objects = scene->getObjects();
    for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	space->addObject(*p);
    }
    space->prepare();
    
    int img_w = img->getWidth() / 2;
    int img_h = img->getHeight() / 2;

    for (double y = -img_h; y < img_h; y++) {
        for (double x = -img_w; x < img_w; x++) {
	    RGB color = getPixel(x,y);
	    img->setRGB((int)x + img_w, (int)(-y) + img_h - 1, color);
	}
    }
}
