
#include "math/vector.h"
#include <vector>

/**
 * A less memoryexpensive replacement for stl::vector<Vector>
 */
class VectorList {

    public:
	VectorList();
	unsigned int size() const { return v.size(); };
	Vector get(const unsigned int i) const;
	unsigned int push_back(const Vector& w);
	Vector operator[](const unsigned int i) const;

    private:
	std::vector<double> v;
};

inline
Vector VectorList::get(unsigned int i) const {
    unsigned int k = i * 3;
    return Vector(v[k],v[k+1],v[k+2]);
}

inline
Vector VectorList::operator[](const unsigned int i) const {
    unsigned int k = i * 3;
    return Vector(v[k],v[k+1],v[k+2]);
}
