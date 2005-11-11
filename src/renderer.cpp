
#include <iostream>
#include <cassert>

#include "renderer.h"
#include "exception.h"
#include "cameras/camera.h"
#include "image/image.h"
#include "scene.h"
#include "image/rgb.h"
#include "image/rgba.h"
#include "objects/object.h"
#include "math/matrix.h"
#include "math/vector2.h"
#include "space/kdtree.h"
#include "materials/material.h"
#include "math/halton.h"

Renderer::Renderer(RendererSettings* settings, Image* img, Scene* scene, KdTree* spc, RenderJobPool* job_pool, uint32_t thread_id) {
    this->scene = scene;
    this->space = spc;
    this->renderersettings = settings;
    this->job_pool = job_pool;
    this->thread_id = thread_id;
    this->img = img;
    this->aborting = false;
    this->gloss_sequence = new Halton(2,2);

    // Prepare the two PixelBlock buffers
    Camera* camera = scene->getCamera();
    aa_enabled = camera->isAAEnabled();
    aa_depth = camera->getAADepth();
    uint32_t block_size = 1 + (1 << aa_depth);
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

Renderer::~Renderer() {
    delete gloss_sequence;
    
    for(uint32_t i = 0; i < row1.size(); i++) {
	row1[i].cleanup();
	row2[i].cleanup();
    }
}

inline
void Renderer::PixelBlock::reset() {
    memset(active,(int)false,size_squared*sizeof(bool));
}

void Renderer::abort() {
    aborting = true;
}

void Renderer::run() {
    RenderJob* job;
    while ((job = job_pool->getJob()) != NULL && !aborting) {
	render(*job);
	job_pool->markJobDone(job);
    }
}

/**
 * Render a job.
 *
 * @param job The job to render
 */
void Renderer::render(const RenderJob& job) {
    if (job.type == RenderJob::NEED_PREVIEW) {
	renderPreview(job);
    } else if (job.type == RenderJob::NEED_FULL_RENDER) {
	renderFull(job);
    } else {
	throw_exception("Unknown type of render job.");
    }
}

/**
 * Find the colors of the corner-pixels of the job and 
 * plot a gradient into the target image.
 *
 * @param job The job to render
 */
void Renderer::renderPreview(const RenderJob& job) {

    // Find the corner colors 
    job.ul = getPixel(Vector2(job.begin_x,job.begin_y));
    job.ur = getPixel(Vector2(job.end_x,job.begin_y));
    job.ll = getPixel(Vector2(job.begin_x,job.end_y));
    job.lr = getPixel(Vector2(job.end_x,job.end_y));
    
    // Plot the gradient
    int w = job.end_x - job.begin_x;
    int h = job.end_y - job.begin_y;
    int img_h = img->getHeight();
    for(int y = 0; y < h; y++) {
	for(int x = 0; x < w; x++) {
	    double dx = double(x) / double (w);
	    double dy = double(y) / double (h);
	    RGBA u = dx * job.ur + (1-dx) * job.ul;
	    RGBA l = dx * job.lr + (1-dx) * job.ll;
	    RGBA c = dy * l + (1-dy) * u;

	    int img_x = x + job.begin_x;
	    int img_y = y + job.begin_y;
	    img->setRGBA(img_x, img_h - img_y - 1, c);
	 //   img->setRGBA(img_x, img_y, job.ul);
	}
    }
    return;
}

/**
 * Find the colors of all pixels of the job and write 
 * into the target image.
 *
 * TODO: Jitter the samples within the grid.
 * 
 * @param job The job to render
 */
void Renderer::renderFull(const RenderJob& job) {
    Camera* camera = scene->getCamera();
    aa_enabled = camera->isAAEnabled();
    aa_depth = camera->getAADepth();
    

    int img_w = img->getWidth();
    int img_h = img->getHeight();

    // Prepare the two PixelBlock buffers
    uint32_t block_size = 1 + (1 << aa_depth);
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
    for (int y = job.begin_y; y < job.end_y && !aborting; y++) {
	if (y != job.begin_y) {
	    // Swap row buffers
	    tmp_row_ptr = cur_row_ptr;
	    cur_row_ptr = prev_row_ptr;
	    prev_row_ptr = tmp_row_ptr;
	    prepareCurRow(cur_row_ptr,prev_row_ptr,block_size);
	}
	for (int x = job.begin_x; x < job.end_x && !aborting; x++) {
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
void Renderer::prepareCurRow(std::vector<PixelBlock>* cur_row, std::vector<PixelBlock>* prev_row,uint32_t blocksize) {
    assert(cur_row->size() == prev_row->size());
    uint32_t width = cur_row->size();

    for(uint32_t i = 0; i < width; i++) {
	PixelBlock& cur_row_block = cur_row->operator[](i);
	PixelBlock& prev_row_block = prev_row->operator[](i);
	cur_row_block.reset();
	
	for(uint32_t j = 0; j < blocksize; j++) {
	    if (prev_row_block.isActive(j,0)) {
		cur_row_block.setColor(j,blocksize-1,prev_row_block.getColor(j,0));
	    }
	}
    }
}

/**
 * Copies rightmost subpixels from prev_block into leftmost subpixels in cur_block
 */
void Renderer::prepareCurBlock(PixelBlock* cur_block, PixelBlock* prev_block, uint32_t blocksize) {
    for(uint32_t i = 0; i < blocksize; i++) {
	if (prev_block->isActive(blocksize-1,i)) {
	    cur_block->setColor(0,i,prev_block->getColor(blocksize-1,i));
	}
    }
}

RGBA Renderer::getSubPixel(uint32_t curLevel, const Vector2& center, PixelBlock *block, double size, int x1, int y1, int x2, int y2) {

    double halfsize = size / 2.0;

    // Jitter
//#define JIT RANDOM(-halfsize/2,halfsize/2)
#define JIT 0

    // Find corner pixels
    Vector2 lowerleft = center - Vector2(halfsize,halfsize) + Vector2(JIT,JIT);
    Vector2 upperright = center + Vector2(halfsize,halfsize) + Vector2(JIT,JIT);
    Vector2 upperleft = Vector2(lowerleft[0],upperright[1]) + Vector2(JIT,JIT);
    Vector2 lowerright = Vector2(upperright[0],lowerleft[1]) + Vector2(JIT,JIT);

#undef JIT


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

Renderer::PixelBlock::PixelBlock(const uint32_t size) {
    this->size = size;
    this->size_squared = size*size;
    color = new RGBA[size*size]; 
    active = new bool[size*size]; 
    reset();
}

void Renderer::PixelBlock::cleanup() {
    delete [] color;
    delete [] active;
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

