#ifndef RENDERER_H
#define RENDERER_H

#include "image/rgba.h"
#include "math/vector.h"
#include "math/vector2.h"
#include "renderersettings.h"
#include "renderjobs.h"
#include "scene.h"
#include "stats.h"

class RGB;
class Image;
class KdTree;
class Material;
class Vector2;
class QMCSequence;
class Sampler;

///  An abstract class all renderers must implement.
class Renderer {

public:
  Renderer(RendererSettings *settings, Image *img, Scene *scene, KdTree *space,
           RenderJobPool *job_pool, uint32_t thread_id);

  /// Run
  void run();

  void abort();

  /// Destructor
  virtual ~Renderer();

  /**
   * The public render-method uses this to render the image. Subclasses must
   * implement this.
   * @param c coordinates in [0,1]x[0,1]
   */
  virtual RGBA getPixel(const Vector2 &c) = 0;

private:
  /// Process a renderjob
  void render(const RenderJob &job);
  void renderPreview(const RenderJob &job);

  Sampler *sampler;

  Image *img;
  RenderJobPool *job_pool;
  uint32_t thread_id;

protected:
  /// The scene to be rendered can be accessed from implementations of Renderer.
  Scene *scene;

  /// The space containing the objects of the scene to render
  KdTree *space;

  /// The settings for the renderer
  RendererSettings *renderersettings;

  Vector2 fresnel(Vector normal, const Vector &ray_dir,
                  const Material *material) const;

  QMCSequence *gloss_sequence;

  bool aborting;
};

#endif
