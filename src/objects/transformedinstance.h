

/**
 * A transformed instance of an object.
 *
 * This allows the same object to be duplicated 
 * around the scene in different positions, with
 * only one instance being kept in memory.
 */
class TransformedInstance : public Object {

    public:
	TransformedInstance(Object* object);
	TransformedInstance(Object* object, Material* material);

	void transform(const Matrix& matrix);
	virtual BoundingBox boundingBoundingBox() const;

    private:
	virtual Intersection _intersect(const Ray& ray) const;
	void prepareMatrices();

	Matrix transformation;
	Matrix inverse_transformation;
	Matrix rotation; /// The rotation part extracted from the transformation
	Matrix inverse_rotation;
	Matrix scene_transformation;
}
