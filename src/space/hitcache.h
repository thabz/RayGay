
#ifndef SPACE_HITCACHE_H
#define SPACE_HITCACHE_H

#include "math/vector2.h"

class Object;

class HitCache {

    public:
	HitCache(unsigned int size);
	~HitCache();

	Object* findEntry(void* fromObject) const;
	void addEntry(void* fromObject, Object* toObject, double t);
	void removeEntry(void* fromObject);

    private:
	int findEntryIndex(void* fromObject) const;

	class Entry {
	    public:
		void* fromObject;
		Object* toObject;
		double t;
	};

	Entry* entries;
	unsigned int size;
	unsigned int first;
};

inline
Object* HitCache::findEntry(void* fromObject) const {
    for(unsigned int i = 0; i < size; i++) {
	if (entries[i].fromObject == fromObject) {
	    return entries[i].toObject;
	}
    }
    return 0;
}

#endif

