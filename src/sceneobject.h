
#ifndef SCENE_OBJECT_H
#define SCENE_OBJECT_H

class Matrix;
class SpaceSubdivider;

/**
 * All that can be added to a scene
 */
class SceneObject {

    public:
	/// Destructor
	virtual ~SceneObject() {};

	/// Prepares the object before rendering
	virtual void prepare() = 0;

	// Transform this object
	virtual void transform(const Matrix& m) = 0;

	// Add self or all subobjects to a space
	virtual void addSelf(SpaceSubdivider* space) = 0;
};

#endif

