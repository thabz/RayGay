
#ifndef OBJECT_COLLECTION_H
#define OBJECT_COLLECTION_H

class Matrix;
class SpaceSubdivider;

class ObjectCollection {

    public:
	/// Transform this object
	virtual void transform(const Matrix& m) = 0;

	/// Adds this or all subobjects to a space
	virtual void addParts(SpaceSubdivider* space) = 0;
};

#endif
