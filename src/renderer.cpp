
#include <iostream>
#include <cassert>

#include "renderer.h"
#include "camera.h"
#include "image/image.h"
#include "scene.h"
#include "image/rgb.h"
#include "image/rgba.h"
#include "objects/object.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "space/kdtree.h"
#include "materials/material.h"

Renderer::Renderer(RendererSettings* settings, Image* img, Scene* scene, KdTree* spc, RenderJobPool* job_pool, unsigned int thread_id) {
    this->scene = scene;
    this->space = spc;
    this->renderersettings = settings;
    this->job_pool = job_pool;
    this->thread_id = thread_id;
    this->img = img;

    // Prepare the two PixelBlock buffers
    Camera* camera = scene->getCamera();
    aa_enabled = camera->isAAEnabled();
    aa_depth = camera->getAADepth();
    unsigned int block_size = 1 + (1 << aa_depth);
    if (aa_enabled) {
	int img_w = img->getWidth();
	row1.reserve(img_w);
	row2.reserve(img_w);
	for(int i = 0; i < img_w; i++) {
	    row1.push_back(PixelBlock(block_size));
	    row2.push_back(PixelBlock(block_size));
	}
    }
}

void Renderer::run() {
    RenderJob job;
    while (job_pool->getJob(&job)) {
	render(job);
    }
}

/**
 * Render the scene into an image.
 *
 * \todo Jitter the samples within the grid.
 * 
 * @param job The job to render
 */
void Renderer::render(const RenderJob& job) {
    Camera* camera = scene->getCamera();
    aa_enabled = camera->isAAEnabled();
    aa_depth = camera->getAADepth();
    

    int img_w = img->getWidth();
    int img_h = img->getHeight();

    // Prepare the two PixelBlock buffers
    // TODO: Move this to constructor
    unsigned int block_size = 1 + (1 << aa_depth);
    if (aa_enabled) {
	for(int i = 0; i < img_w; i++) {
	    row1[i].reset();
	    row2[i].reset();
	}
    }

    std::vector<PixelBlock>* cur_row_ptr = &row1;
    std::vector<PixelBlock>* prev_row_ptr = &row2;
    std::vector<PixelBlock>* tmp_row_ptr;
    PixelBlock* cur_block;
    PixelBlock* prev_block;
    RGBA color;
    for (int y = job.begin_y; y < job.end_y; y++) {
	if (y != job.begin_y) {
	    // Swap row buffers
	    tmp_row_ptr = cur_row_ptr;
	    cur_row_ptr = prev_row_ptr;
	    prev_row_ptr = tmp_row_ptr;
	    prepareCurRow(cur_row_ptr,prev_row_ptr,block_size);
	}
	for (int x = job.begin_x; x < job.end_x; x++) {
	    if (aa_enabled) {
		cur_block = &((*cur_row_ptr)[x]);
		if (x != job.begin_x) {
		    prev_block = &((*cur_row_ptr)[x - 1]);
		    prepareCurBlock(cur_block,prev_block,block_size);
		}
		color = getSubPixel(0, Vector2(x,y), cur_block, 1.0, 0, 0, block_size - 1, block_size - 1);
	    } else {
		color = getPixel(Vector2(x,y));
	    }
	    img->setRGBA((int)x, (int)img_h - y - 1, color);
	}
    }
}

/**
 * Clear cur_row and copy lowermost color values from prev_row into topmost color values in cur_row
 */
void Renderer::prepareCurRow(std::vector<PixelBlock>* cur_row, std::vector<PixelBlock>* prev_row,unsigned int blocksize) {
    assert(cur_row->size() == prev_row->size());
    PixelBlock* cur_row_block;
    PixelBlock* prev_row_block;
    unsigned int width = cur_row->size();

    for(unsigned int i = 0; i < width; i++) {
	cur_row_block = &((*cur_row)[i]);
	prev_row_block = &((*prev_row)[i]);
	cur_row_block->reset();
	
	for(unsigned int j = 0; j < blocksize; j++) {
	    if (prev_row_block->isActive(j,0)) {
		cur_row_block->setColor(j,blocksize-1,prev_row_block->getColor(j,0));
	    }
	}
    }
}

/**
 * Copies rightmost subpixels from prev_block into leftmost subpixels in cur_block
 */
void Renderer::prepareCurBlock(PixelBlock* cur_block, PixelBlock* prev_block, unsigned int blocksize) {
    for(unsigned int i = 0; i < blocksize; i++) {
	if (prev_block->isActive(blocksize-1,i)) {
	    cur_block->setColor(0,i,prev_block->getColor(blocksize-1,i));
	}
    }
}

RGBA Renderer::getSubPixel(unsigned int curLevel, const Vector2& center, PixelBlock *block, double size, int x1, int y1, int x2, int y2) {

    double halfsize = size / 2.0;

    // Find corner pixels
    Vector2 lowerleft = center - Vector2(halfsize,halfsize);
    Vector2 upperright = center + Vector2(halfsize,halfsize);
    Vector2 upperleft = Vector2(lowerleft[0],upperright[1]);
    Vector2 lowerright = Vector2(upperright[0],lowerleft[1]);

    RGBA c1,c2,c3,c4;
	
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
    return (c1 + c2 + c3 + c4) * 0.25;
}

Renderer::PixelBlock::PixelBlock(const unsigned int size) {
    this->size = size;
    this->size_squared = size*size;
    color = new RGBA[size*size]; 
    active = new bool[size*size]; 
    reset();
}


inline
void Renderer::PixelBlock::reset() {
    memset(active,(int)false,size_squared*sizeof(bool));
}

/**
 * The Fresnel equation.
 *
 * Schlicks approximation to the Fresnel equation is
 *
 * \f[ R(N,V) = R_0 + \left(1-R_0\right)\left(1-\left(N \cdot V\right)\right)^5 \f]
 *
 * where the reflectance \f$R_0\f$ is
 *
 * \f[ R_0 = \frac{(1-\eta)^2}{(1+\eta)^2} \f]
 *
 * @param normal The surface normal which is \f$N\f$
 * @param ray_dir Ray direction which is \f$V\f$
 * @param material The material of the surface
 *
 * @return (reflection,transmission)
 */
Vector2 Renderer::fresnel(Vector normal, const Vector& ray_dir, const Material* material) const {
    double reflectance,reflection,transmission,eta;

    if (material->getKt() > 0.0) {
	if (normal * ray_dir > 0) {
	    eta = material->getEta();
	    normal *= -1;
	} else {
	    eta = 1.0 / material->getEta();
	}
	reflectance = ((1 - eta) * (1 - eta)) / ((1 + eta) * (1 + eta));
    } else {
	reflectance = material->getKs();
    }

    double nv = -(normal * ray_dir);

    if (nv > EPSILON) {
	reflection = reflectance + (1 - reflectance) * pow(1-nv,5);
	transmission = material->getKd() + material->getKs() + material->getKt() - reflection;
    } else {
	reflection = 0;
	transmission = 0;
    }
    return Vector2(reflection,transmission);

}

void getTile(int tile_num, int width, int height, int* begin_x, int* end_x, int*  begin_y, int* end_y) {

}

	
