
#include <iostream>

#include "renderer.h"
#include "image/image.h"
#include "scene.h"
#include "image/rgb.h"
#include "object.h"
#include "spacesubdivider.h"
#include "objectcollection.h"

Renderer::Renderer() {
}

void Renderer::render(Scene* sc, Image* img, SpaceSubdivider* spc) {
    scene = sc;
    space = spc;

    // Add all objects in scene to spacesubdivider
    std::vector<object*> objects = scene->getObjects();
    for (vector<object*>::iterator p = objects.begin(); p != objects.end(); p++) {
	(*p)->prepare();
	space->addObject(*p);
    }
    //
    // Add all objectcollections in scene to spacesubdivider
    std::vector<ObjectCollection*> objectcols = scene->getObjectCollections();
    for (vector<ObjectCollection*>::iterator p = objectcols.begin(); p != objectcols.end(); p++) {
	(*p)->addParts(space);
    }


    space->prepare();
    std::cout << "Prepare done" << std::endl;
    
    int img_w = img->getWidth() / 2;
    int img_h = img->getHeight() / 2;

    for (double y = -img_h; y < img_h; y++) {
        for (double x = -img_w; x < img_w; x++) {
	    RGB color = getPixel(x,y);
	    img->setRGB((int)x + img_w, (int)(-y) + img_h - 1, color);
	}
    }
}
