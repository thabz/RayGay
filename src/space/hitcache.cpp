
#include "space/hitcache.h"
#include "types.h"
#include <cstdlib>

HitCache::HitCache(uint size) {
    this->size = size;
    this->first = 0;
    entries = new Entry[size];
}

HitCache::~HitCache() {
    delete [] entries;
}

inline
int HitCache::findEntryIndex(void* fromObject) const {
    for(uint i = 0; i < size; i++) {
	if (entries[i].fromObject == fromObject)
	    return i;
    }
    return -1;
}


void HitCache::addEntry(void* fromObject, Object* toObject, double t) {
    int i = findEntryIndex(fromObject);
    if (i == -1) {
	first = (first + 1 ) % size;
	entries[first].fromObject = fromObject;
	entries[first].toObject = toObject;
	entries[first].t = t;
    } else {
	entries[i].toObject = toObject;
	entries[i].t = t;
    }
}

void HitCache::removeEntry(void* fromObject) {
    int i = findEntryIndex(fromObject);
    if (i == -1)
	return;
    entries[i].fromObject = NULL;
}


