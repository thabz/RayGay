#ifndef BOOLEAN_OPERAND_H
#define BOOLEAN_OPERAND_H

#include "object.h"

class Vector;

/// A type of object that can be used as part of a Boolean object
class BooleanOperand : public object {

    public:
	virtual bool onEdge(const Vector &p) const = 0;
	virtual bool inside(const Vector &p) const = 0;

};

#endif
