#ifndef RENDERER_H
#define RENDERER_H

#include "scene.h"
#include "stats.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "image/rgba.h"
#include "renderersettings.h"
#include "renderjobs.h"

//#define aa_threshhold 0.02
#define aa_threshhold 0.3

class RGB;
class Image;
class KdTree;
class Material;
class Vector2;


///  An abstract class all renderers must implement.
class Renderer {

    public:
	Renderer(RendererSettings* settings, Image* img, Scene* scene, KdTree* space, RenderJobPool* job_pool, unsigned int thread_id);

	/// Run
	void run();

	void abort();

	/// Destructor
	virtual ~Renderer();


    private:
	/// The public render-method uses this to render the image. Subclasses must implement this.
	virtual RGBA getPixel(const Vector2& c) = 0; 

	class PixelBlock {
	    public:
		PixelBlock(const unsigned int size);
		void cleanup();
		void reset();
		bool isActive(const int x, const int y) const { return active[y*size + x]; };
		void setColor(const int x, const int y, const RGBA& c) { color[y*size + x] = c; active[y*size + x] = true;};
		RGBA getColor(const int x, const int y) const { return color[y*size + x]; };
	    private:
	        RGBA* color;
		bool* active;
		unsigned int size;
		unsigned int size_squared;
	};
	
	/// Process a renderjob
	void render(const RenderJob& job);
	void renderPreview(const RenderJob& job);
	void renderFull(const RenderJob& job);

	RGBA getSubPixel(unsigned int curLevel, const Vector2& center, PixelBlock *block, double size, int x1, int y1, int x2, int y2);
	void prepareCurRow(std::vector<PixelBlock>* cur_row, std::vector<PixelBlock>* prev_row, unsigned int blocksize);
	void prepareCurBlock(PixelBlock* cur_block, PixelBlock* prev_block, unsigned int blocksize);

	bool aa_enabled;
	unsigned int aa_depth;
	std::vector<PixelBlock> row1;
	std::vector<PixelBlock> row2;

	Image* img;
	RenderJobPool* job_pool;
	unsigned int thread_id;

    protected:
	/// The scene to be rendered can be accessed from implementations of Renderer.
	Scene* scene;

	/// The space containing the objects of the scene to render
	KdTree* space;

	/// The settings for the renderer
	RendererSettings* renderersettings;


	Vector2 fresnel(Vector normal, const Vector& ray_dir, const Material* material) const;

	bool aborting;
};

#endif
