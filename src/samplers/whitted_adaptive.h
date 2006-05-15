
#ifndef SAMPLERS_WHITTED_ADAPTIVE
#define SAMPLERS_WHITTED_ADAPTIVE

#include "samplers/sampler.h"
#include <vector>

/**
 * Implementation of Whitted's Adaptive Supersampling.
 *
 * The technique fires rays at pixel corners as well as their center
 * If all five rays return about the same color, they can be averaged
 * to produce the pixel's color.
 * If they return different colors, we'll subdivide the pixel into smaller
 * regions and treat the subpixel just as we did the entire pixel.
 * The number of subdivisions can be stopped at some maximum level.
 *
 * @see Whitted, Turner, "An Improved Illumination Model for Shaded Display", Comm.
ACM, 23(6), June 1980, pp. 343-349.
 */
class WhittedAdaptive : public Sampler 
{
    public:
	WhittedAdaptive(Image* image, Renderer* renderer, uint32_t aa_depth);
	~WhittedAdaptive();
	Sampler* clone();

	virtual void render(const RenderJob& job);

    private:
	uint32_t aa_depth;
	int img_w, img_h;

	class PixelBlock {
	    public:
		PixelBlock(const uint32_t size);
		void cleanup();
		void reset();
		bool isActive(const int x, const int y) const { return active[y*size + x]; };
		void setColor(const int x, const int y, const RGBA& c) { color[y*size + x] = c; active[y*size + x] = true;};
		RGBA getColor(const int x, const int y) const { return color[y*size + x]; };
	    private:
	        RGBA* color;
		bool* active;
		uint32_t size;
		uint32_t size_squared;
	};

	std::vector<PixelBlock> row1;
	std::vector<PixelBlock> row2;

	RGBA getSubPixel(uint32_t curLevel, const Vector2& center, PixelBlock *block, double size, int x1, int y1, int x2, int y2);
	void prepareCurRow(std::vector<PixelBlock>* cur_row, std::vector<PixelBlock>* prev_row, uint32_t blocksize);
	void prepareCurBlock(PixelBlock* cur_block, PixelBlock* prev_block, uint32_t blocksize);


};

#endif
