
#ifndef OBJECTS_OBJECTGROUP_H
#define OBJECTS_OBJECTGROUP_H

#include "sceneobject.h"
#include <vector>

/// An object that builds itself from other objects
class ObjectGroup : public SceneObject {

    public:
	/// Transform this object
	virtual void transform(const Matrix& m);

	/// Adds this or all subobjects to a space
	virtual void addSelf(SpaceSubdivider* space);

	/// Prepare object
	void prepare();

    protected:
	/// Add an object to this group
	void addObject(SceneObject* obj);

    private:
	std::vector<SceneObject*> objects;
};

#endif
