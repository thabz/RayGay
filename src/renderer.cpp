
#include <iostream>
#include <time.h>

#include "renderer.h"
#include "image/image.h"
#include "scene.h"
#include "image/rgb.h"
#include "object.h"
#include "spacesubdivider.h"
#include "objectcollection.h"

Renderer::Renderer() {
}

/**
 * Render the scene into an image
 * 
 * @param sc The scene to render
 * @param img The image to place pixels on
 * @param spc The space containing the objects of the scene
 */
void Renderer::render(Scene* sc, Image* img, SpaceSubdivider* spc) {
    time_t beginTime;
    scene = sc;
    space = spc;

    beginTime = time(NULL);
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
	(*p)->prepare();
	(*p)->addParts(space);
    }
    space->prepare();
    Stats::getUniqueInstance()->put("Prepare time (seconds)",time(NULL)-beginTime);
    
    beginTime = time(NULL);
    int img_w = img->getWidth() / 2;
    int img_h = img->getHeight() / 2;

    for (double y = -img_h; y < img_h; y++) {
        for (double x = -img_w; x < img_w; x++) {
	    RGB color = getPixel(x,y);
	    img->setRGB((int)x + img_w, (int)(-y) + img_h - 1, color);
	}
    }
    Stats::getUniqueInstance()->put("Rendering time (seconds)",time(NULL)-beginTime);
}

