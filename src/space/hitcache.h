
#ifndef SPACE_HITCACHE_H
#define SPACE_HITCACHE_H

class Object;

class HitCache {

    public:
	HitCache(unsigned int size);
	~HitCache();

	Object* findEntry(void* fromObject) const;
	void addEntry(void* fromObject, Object* toObject, double t);

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

#endif

