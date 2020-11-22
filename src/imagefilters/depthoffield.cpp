
#include "image/image.h"
#include "image/rgb.h"

#include "depthoffield.h"

DepthOfField::DepthOfField(Image *depth_buffer, double focus_depth,
                           double near_radius, double far_radius) {
  this->depth_buffer = depth_buffer;
  this->focus_depth = focus_depth;
  this->near_radius = near_radius;
  this->far_radius = far_radius;
}

void DepthOfField::apply(Image *image) {}

RGB DepthOfField::sampleCircle(int x, int y, double z, double radius,
                               double tolerance, Image *image) const {

  int int_rad = int(radius);
  double rad2 = radius * radius;

  int x_min = MAX(x - int_rad, 0);
  int x_max = MIN(x + int_rad, image->getWidth() - 1);

  int y_min = MAX(y - int_rad, 0);
  int y_max = MIN(y + int_rad, image->getHeight() - 1);

  int samples = 0;
  RGB result = RGB(0.0, 0.0, 0.0);

  for (int b = y_min; b < y_max; b++) {
    for (int a = x_min; a < x_max; a++) {
      if ((x - a) * (x - a) + (y - b) * (y - b) <= rad2) {
        RGB depth = depth_buffer->getRGBA(a, b);
        if (fabs(depth.r() - z) < tolerance) {
          result += image->getRGBA(a, b);
          samples++;
        }
      }
    }
  }
  if (samples > 0) {
    return result / double(samples);
  }

  return result;
}
