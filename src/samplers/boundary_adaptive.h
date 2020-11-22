
#ifndef SAMPLERS_BOUNDARY_ADAPTIVE
#define SAMPLERS_BOUNDARY_ADAPTIVE

#include "samplers/sampler.h"
#include <vector>

class BoundaryAdaptiveFactory : public SamplerFactory {
public:
  BoundaryAdaptiveFactory(uint32_t aa_depth);
  Sampler *createInstance(Image *img, Renderer *renderer);

private:
  uint32_t aa_depth;
};

/**
 */
class BoundaryAdaptive : public Sampler {
  friend Sampler *BoundaryAdaptiveFactory::createInstance(Image *, Renderer *);

public:
  ~BoundaryAdaptive();

  virtual void render(const RenderJob &job);

private:
  BoundaryAdaptive(Image *image, Renderer *renderer, uint32_t aa_depth);
  uint32_t aa_depth;
  int img_w, img_h;

  class Pixel {
  public:
    Vector2 pos;
    RGBA color;
    bool *active;
  };
  class PixelList {
  public:
    vector<Pixel> list;
    void add(Vector2, RGBA color);
    void add(const Pixel &pixel);
    const Pixel &last();
    const Pixel &first();
  };
  class PixelBlock {
  public:
    PixelList left;
    PixelList right;
    PixelList top;
    PixelList bottom;
    Vector2 center;
  };

  std::vector<PixelBlock> row1;
  std::vector<PixelBlock> row2;

  RGBA getSubPixel(uint32_t curLevel, const Vector2 &center, PixelBlock *block,
                   double size, int x1, int y1, int x2, int y2);
  void prepareCurRow(std::vector<PixelBlock> *cur_row,
                     std::vector<PixelBlock> *prev_row, uint32_t blocksize);
  void prepareCurBlock(PixelBlock *cur_block, PixelBlock *prev_block,
                       uint32_t blocksize);
};

#endif
