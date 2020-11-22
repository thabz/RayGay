
#ifndef OBJECTS_HEIGHT_FIELD_H
#define OBJECTS_HEIGHT_FIELD_H

#include "objects/parametrizedsurface.h"

class Texture;

/**
 * A heightfield.
 */
class HeightField : public ParametrizedSurface {
public:
  HeightField(Texture *texture, double height, double width, double depth,
              uint32_t width_divisions, uint32_t depth_divisions,
              const Material *material);

protected:
  Vector eval(double u, double v) const;

private:
  double height, width, depth;
  Texture *texture;
};

#endif
