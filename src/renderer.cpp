
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

    aa_enabled = scene->getCamera()->isAAEnabled();
    aa_depth = scene->getCamera()->getAADepth();
    
    beginTime = time(NULL);
    int img_w = img->getWidth() / 2;
    int img_h = img->getHeight() / 2;

    RGB color;
    PixelBlock* block;

    unsigned int block_size = 1 + (1 << aa_depth);

    if (aa_enabled) {
	cout << "Block size: " << block_size << endl;
	cout << "aa_depth: " << aa_depth << endl;
	block = new PixelBlock(block_size);
    }

    for (double y = -img_h; y < img_h; y++) {
	for (double x = -img_w; x < img_w; x++) {
	    if (aa_enabled) {
		block->reset();
		color = getSubPixel(0, Vector2(x,y), block, 1.0, 0, 0, block_size - 1, block_size - 1);
	    } else {
		color = getPixel(Vector2(x,y));
	    }
	    img->setRGB((int)x + img_w, (int)(-y) + img_h - 1, color);
	}
	cout << y + img_h << " / " << img_h*2 << "          \r" << flush;
    }
    Stats::getUniqueInstance()->put("Rendering time (seconds)",time(NULL)-beginTime);
}

RGB Renderer::getSubPixel(unsigned int curLevel, const Vector2& center, PixelBlock *block, double size, int x1, int y1, int x2, int y2) {

    double halfsize = size / 2.0;

    // Find corner pixels
    Vector2 lowerleft = center - Vector2(halfsize,halfsize);
    Vector2 upperright = center + Vector2(halfsize,halfsize);
    Vector2 upperleft = Vector2(lowerleft[0],upperright[1]);
    Vector2 lowerright = Vector2(upperright[0],lowerleft[1]);

    RGB c1,c2,c3,c4;

	
    // Trace upper left corner
    if (!block->isActive(x1,y1)) {
	c1 =  getPixel(upperleft);
	block->setColor(x1,y1,c1);
    } else {
	c1 = block->getColor(x1,y1);
    }

    // Trace lower left corner
    if (!block->isActive(x1,y2)) {
	c2 = getPixel(lowerleft);
	block->setColor(x1,y2,c2);
    } else {
	c2 = block->getColor(x1,y2);
    }

    // Trace upper right corner
    if (!block->isActive(x2,y1)) {
	c3 = getPixel(upperright);
	block->setColor(x2,y1,c3);
    } else {
	c3 = block->getColor(x2,y1);
    }
    
    // Trace lower right corner
    if (!block->isActive(x2,y2)) {
	c4 = getPixel(lowerright);
	block->setColor(x2,y2,c4);
    } else {
	c4 = block->getColor(x2,y2);
    }

    
    // Check if we need further supersampling
    if (aa_enabled && curLevel <= aa_depth) {
	if (c1.sqrDistance(c2) >= aa_threshhold ||
		c2.sqrDistance(c3) >= aa_threshhold ||
		c3.sqrDistance(c4) >= aa_threshhold ||
		c4.sqrDistance(c1) >= aa_threshhold ||
		c1.sqrDistance(c3) >= aa_threshhold ||
		c2.sqrDistance(c4) >= aa_threshhold) {

	    // Center of this sub-block
	    int xc = (x1 + x2) / 2;
	    int yc = (y1 + y2) / 2;

	    // Trace the four sub-blocks
	    c1 = getSubPixel(curLevel+1,(upperleft+center)*0.5,block,halfsize,x1,y1,xc,yc);
	    c2 = getSubPixel(curLevel+1,(lowerright+center)*0.5,block,halfsize,xc,yc,x2,y2);
	    c3 = getSubPixel(curLevel+1,(lowerleft+center)*0.5,block,halfsize,x1,yc,xc,y2);
	    c4 = getSubPixel(curLevel+1,(upperright+center)*0.5,block,halfsize,xc,y1,x2,yc);
	}
    }

    // Return average
    return 0.25 * (c1 + c2 + c3 + c4);
}

Renderer::PixelBlock::PixelBlock(const unsigned int size) {
    this->size = size;
    color = new RGB[size*size]; 
    active = new bool[size*size]; 
    reset();
}

void Renderer::PixelBlock::reset() {
    for(unsigned int i = 0; i < size*size; i++) {
	active[i] = false;
    }
}

