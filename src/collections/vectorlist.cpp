
#include "vectorlist.h"

unsigned int VectorList::push_back(const Vector& w) {
    v.push_back(w[0]);
    v.push_back(w[1]);
    v.push_back(w[2]);
    return v.size() - 1;
}

