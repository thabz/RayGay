
#ifndef OBJECTS_OBJECTGROUP_H
#define OBJECTS_OBJECTGROUP_H

#include "objectcollection.h"
#include <vector>

class object;

/// An object that builds itself from other objects
class ObjectGroup : public ObjectCollection {

    public:
	/// Transform this object
	void transform(const Matrix& m);

	/// Adds this or all subobjects to a space
	void addParts(SpaceSubdivider* space);

	/// Prepare object
	void prepare();

    protected:
	/// Add an object to this group
	void addObject(object* obj);

    private:
	std::vector<object*> objects;
};

#endif
