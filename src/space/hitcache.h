
#ifndef SPACE_HITCACHE_H
#define SPACE_HITCACHE_H

class HitCache {

    public:
	HitCache(unsigned int size);
	~HitCache();

	void* findEntry(void* fromObject) const;
	void addEntry(void* fromObject, void* toObject, double t);

    private:
	int findEntryIndex(void* fromObject) const;

	class Entry {
	    public:
		void* fromObject;
		void* toObject;
		double t;
	};

	Entry* entries;
	unsigned int size;
	unsigned int first;
};

#endif

