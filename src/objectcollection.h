
#ifndef OBJECT_COLLECTION_H
#define OBJECT_COLLECTION_H

class Matrix;
class SpaceSubdivider;

/// An object that create other objects to be rendered.
class ObjectCollection {

    public:
	virtual ~ObjectCollection() {};

	/// Transform this object
	virtual void transform(const Matrix& m) = 0;

	/// Adds this or all subobjects to a space
	virtual void addParts(SpaceSubdivider* space) = 0;

	/// Prepare object
	virtual void prepare() = 0;
};

#endif
