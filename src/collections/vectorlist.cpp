
#include "vectorlist.h"
#include "math/matrix.h"

unsigned int VectorList::push_back(const Vector& w) {
    v.push_back(w[0]);
    v.push_back(w[1]);
    v.push_back(w[2]);
    return size() - 1;
}

void VectorList::transform(const Matrix& M) {
    uint size = this->size();
    for (uint i = 0; i < size; i++) {
	set(i, M * get(i));
    }
}

void VectorList::reserve(uint num) {
    v.reserve(num * 3);
}
