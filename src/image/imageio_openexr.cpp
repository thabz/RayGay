#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_OPENEXR

#include "exception.h"
#include "image/imageimpl.h"
#include "image/imageio_openexr.h"

#include <ImfArray.h>
#include <ImfRgbaFile.h>
#include <exception>
#include <string>

using namespace OPENEXR_IMF_NAMESPACE;

void OpenExrIO::save(const Image *const image,
                     const std::string &filename) const {
  const int width = image->getWidth();
  const int height = image->getHeight();

  Array2D<Rgba> pixels;
  pixels.resizeErase(height, width);

  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      RGBA color = image->getRGBA(x, y);
      pixels[y][x] = Rgba(color.r(), color.g(), color.b(), color.a());
    }
  }

  try {
    RgbaOutputFile file(filename.c_str(), width, height, WRITE_RGBA);
    file.setFrameBuffer(&pixels[0][0], 1, width);
    file.writePixels(height);
  } catch (const std::exception &e) {
    throw_exception("Error saving OpenEXR file " + filename + ": " + e.what());
  }
}

void OpenExrIO::save(const Image *const image, FILE *file) const {
  throw_exception("OpenEXR saving requires a filename.");
}

Image *OpenExrIO::load(const std::string &filename, Allocator::model_t model) {
  try {
    RgbaInputFile file(filename.c_str());
    Imath::Box2i data_window = file.dataWindow();
    int width = data_window.max.x - data_window.min.x + 1;
    int height = data_window.max.y - data_window.min.y + 1;

    Array2D<Rgba> pixels;
    pixels.resizeErase(height, width);
    file.setFrameBuffer(&pixels[0][0] - data_window.min.x -
                            data_window.min.y * width,
                        1, width);
    file.readPixels(data_window.min.y, data_window.max.y);

    Image *result = new ImageImpl<float, 4>(width, height, model);
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        const Rgba &pixel = pixels[y][x];
        result->setRGBA(x, y, RGBA(pixel.r, pixel.g, pixel.b, pixel.a));
      }
    }
    return result;
  } catch (const std::exception &e) {
    throw_exception("Error reading OpenEXR file " + filename + ": " +
                    e.what());
  }
}

#endif /* HAVE_OPENEXR */
