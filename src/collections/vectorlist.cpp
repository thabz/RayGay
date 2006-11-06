
#include "vectorlist.h"
#include "math/matrix.h"
#include "math/matrix3.h"

VectorList::VectorList() {
}

uint32_t VectorList::push_back(const Vector& w) {
    v.push_back(w[0]);
    v.push_back(w[1]);
    v.push_back(w[2]);
    return size() - 1;
}

void VectorList::transform(const Matrix& M) {
    uint32_t size = this->size();
    for (uint32_t i = 0; i < size; i++) {
	set(i, M * get(i));
    }
}

void VectorList::transform(const Matrix3& M) {
    uint32_t size = this->size();
    for (uint32_t i = 0; i < size; i++) {
	set(i, M * get(i));
    }
}

void VectorList::reserve(uint32_t num) {
    v.reserve(num * 3);
}

void VectorList::normalize() {
    uint32_t size = this->size();
    for (uint32_t i = 0; i < size; i++) {
        Vector v = get(i);
        v.normalize();    
    	set(i, v);
    }
        
}