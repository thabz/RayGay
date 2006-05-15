
#include "samplers/whitted_adaptive.h"
#include "renderer.h"
#include "image/image.h"
#include "renderjobs.h"
#include "math/vector2.h"

WhittedAdaptive::WhittedAdaptive(Image* image, Renderer* renderer, uint32_t aa_depth) : Sampler(image,renderer) 
{
    img_w = image->getWidth();
    img_h = image->getHeight();

    this->aa_depth = aa_depth;

    // Prepare the two PixelBlock buffers
    uint32_t block_size = 1 + (1 << aa_depth);
    int img_w = image->getWidth();
    row1.reserve(img_w);
    row2.reserve(img_w);
    for(int i = 0; i < img_w; i++) {
	row1.push_back(PixelBlock(block_size));
	row2.push_back(PixelBlock(block_size));
    }
}

WhittedAdaptive::~WhittedAdaptive() 
{
    for(uint32_t i = 0; i < row1.size(); i++) {
	row1[i].cleanup();
	row2[i].cleanup();
    }

}

/**
 * Find the colors of all pixels of the job and write 
 * into the target image.
 *
 * TODO: Jitter the samples within the grid.
 * 
 * @param job The job to render
 */
void WhittedAdaptive::render(const RenderJob& job) {

    // Prepare the two PixelBlock buffers
    uint32_t block_size = 1 + (1 << aa_depth);
    for(int i = 0; i < img_w; i++) {
	row1[i].reset();
	row2[i].reset();
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
	    cur_block = &((*cur_row_ptr)[x]);
	    if (x != job.begin_x) {
		prev_block = &((*cur_row_ptr)[x - 1]);
		prepareCurBlock(cur_block,prev_block,block_size);
	    }
	    color = getSubPixel(0, Vector2(x,y), cur_block, 1.0, 0, 0, block_size - 1, block_size - 1);
	    setPixel(x,y,color);
	}
    }
}


/**
 * Clear cur_row and copy lowermost color values from prev_row into topmost color values in cur_row
 */
void WhittedAdaptive::prepareCurRow(std::vector<PixelBlock>* cur_row, std::vector<PixelBlock>* prev_row,uint32_t blocksize) {
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
void WhittedAdaptive::prepareCurBlock(PixelBlock* cur_block, PixelBlock* prev_block, uint32_t blocksize) {
    for(uint32_t i = 0; i < blocksize; i++) {
	if (prev_block->isActive(blocksize-1,i)) {
	    cur_block->setColor(0,i,prev_block->getColor(blocksize-1,i));
	}
    }
}

RGBA WhittedAdaptive::getSubPixel(uint32_t curLevel, const Vector2& center, PixelBlock *block, double size, int x1, int y1, int x2, int y2) {

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
	c1 =  sample(upperleft);
	block->setColor(x1,y1,c1);
    } else {
	c1 = block->getColor(x1,y1);
    }

    // Trace lower left corner
    if (!block->isActive(x1,y2)) {
	c2 = sample(lowerleft);
	block->setColor(x1,y2,c2);
    } else {
	c2 = block->getColor(x1,y2);
    }

    // Trace upper right corner
    if (!block->isActive(x2,y1)) {
	c3 = sample(upperright);
	block->setColor(x2,y1,c3);
    } else {
	c3 = block->getColor(x2,y1);
    }

    // Trace lower right corner
    if (!block->isActive(x2,y2)) {
	c4 = sample(lowerright);
	block->setColor(x2,y2,c4);
    } else {
	c4 = block->getColor(x2,y2);
    }

    
    // Check if we need further supersampling
    if (curLevel <= aa_depth) {
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

WhittedAdaptive::PixelBlock::PixelBlock(const uint32_t size) {
    this->size = size;
    this->size_squared = size*size;
    color = new RGBA[size*size]; 
    active = new bool[size*size]; 
    reset();
}

void WhittedAdaptive::PixelBlock::cleanup() {
    delete [] color;
    delete [] active;
}

inline
void WhittedAdaptive::PixelBlock::reset() {
    memset(active,(int)false,size_squared*sizeof(bool));
}

Sampler* WhittedAdaptive::clone() 
{
    return new WhittedAdaptive(image,renderer,aa_depth);
}
