#ifndef RENDERER_H
#define RENDERER_H

#include "scene.h"
#include "stats.h"
#include "math/vector2.h"

#define aa_threshhold 0.02

class RGB;
class Image;
class SpaceSubdivider;
class PhotonMap;

///  An abstract class all renderers must implement.
class Renderer {

    public:
	/// Render a scene into an image
	void render(Scene* scene, Image*, SpaceSubdivider* space, PhotonMap* photonmap);

	/// Destructor
	virtual ~Renderer() {};

    private:
	/// The public render-method uses this to render the image. Subclasses must implement this.
	virtual RGB getPixel(const Vector2& c) = 0; 

	class PixelBlock {
	    public:
		PixelBlock(const unsigned int size);
		void reset();
		bool isActive(const int x, const int y) const { return active[y*size + x]; };
		void setColor(const int x, const int y, const RGB& c) { color[y*size + x] = c; active[y*size + x] = true;};
		RGB getColor(const int x, const int y) const { return color[y*size + x]; };
	    private:
	        RGB* color;
		bool* active;
		unsigned int size;
	};

	RGB getSubPixel(unsigned int curLevel, const Vector2& center, PixelBlock *block, double size, int x1, int y1, int x2, int y2);
	void prepareCurRow(std::vector<PixelBlock>* cur_row, std::vector<PixelBlock>* prev_row, unsigned int blocksize);
	void prepareCurBlock(PixelBlock* cur_block, PixelBlock* prev_block, unsigned int blocksize);

	bool aa_enabled;
	unsigned int aa_depth;
	std::vector<PixelBlock> row1;
	std::vector<PixelBlock> row2;

    protected:
	Renderer();
	/// The scene to be rendered can be accessed from implementations of Renderer.
	Scene* scene;

	/// The photonmap to use
	PhotonMap* photonmap;

	/// The space containing the objects of the scene to render
	SpaceSubdivider* space;
};

#endif
