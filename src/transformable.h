#ifndef TRANSFORMABLE_H
#define TRANSFORMABLE_H

#include "matrix.h"

class transformable{
    public:
	/// Derived classes should supply this
	virtual void transform(const Matrix &m) = 0;
};

#endif
