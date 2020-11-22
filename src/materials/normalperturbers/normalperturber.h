
#ifndef NORMAL_PERTURBER_H
#define NORMAL_PERTURBER_H

#include "math/matrix.h"
#include "math/vector.h"
#include "transformer.h"

class NormalPerturber : public Transformer {

public:
  Vector perturb(const Vector &point, const Vector &normal) const;
  void transform(const Matrix &m);
  virtual ~NormalPerturber(){};

protected:
  virtual Vector _perturb(const Vector &point, const Vector &normal) const = 0;
};

#endif
