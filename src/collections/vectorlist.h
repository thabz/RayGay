
#ifndef VECTORLIST_H
#define VECTORLIST_H

#include "math/vector.h"
#include <vector>

class Matrix;

/**
 * A less memoryexpensive replacement for stl::vector<Vector>
 */
class VectorList {

    public:
	VectorList();
	unsigned int size() const { return v.size(); };
	Vector get(const unsigned int i) const;
	void get(const unsigned int i, Vector& dest) const;
	void get(const unsigned int i, double dest[3]) const;
	void set(const unsigned int i, Vector v);
	unsigned int push_back(const Vector& w);
        void transform(const Matrix& M);
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
void VectorList::get(unsigned int i, Vector& dest) const {
    unsigned int k = i * 3;
    dest[0] = v[k+0];
    dest[1] = v[k+1];
    dest[2] = v[k+2];
}

inline
void VectorList::get(unsigned int i, double dest[3]) const {
    unsigned int k = i * 3;
    dest[0] = v[k+0];
    dest[1] = v[k+1];
    dest[2] = v[k+2];
}

inline
void set(const unsigned int i, Vector v) {
    unsigned int k = i * 3;
    v[k+0] = v[0];
    v[k+1] = v[1];
    v[k+2] = v[2];
}

inline
Vector VectorList::operator[](const unsigned int i) const {
    unsigned int k = i * 3;
    return Vector(v[k],v[k+1],v[k+2]);
}

#endif
