

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

    private:
	Matrix instance_transform;
}
