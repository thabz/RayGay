
#ifndef LIGHTS_SHADOW_CACHE_H
#define LIGHTS_SHADOW_CACHE_H

#define LIGHTS_SHADOW_CACHE_MAX_DEPTH 32

class Object;

class ShadowCache {
    public:
	ShadowCache();
	Object* get(unsigned int depth) const;
	void put(unsigned int depth, Object* object);

    private:
	Object* objectlist[LIGHTS_SHADOW_CACHE_MAX_DEPTH];

	
};

#endif

