#ifndef BOOLEAN_OPERAND_H
#define BOOLEAN_OPERAND_H

#include "object.h"

class Vector;

/// A type of object that can be used as part of a Boolean object
class BooleanOperand : public Object {

    public:
	BooleanOperand(const Material* mat);
	/// Says whether a point belongs to the closure of the object
	virtual bool onEdge(const Vector &p) const = 0;
	
	/// Says whether a point belongs to the strict inside of the object
	virtual bool inside(const Vector &p) const = 0;

};


#endif
