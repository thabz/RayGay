
#ifndef LIGHTS_SHADOW_CACHE_H
#define LIGHTS_SHADOW_CACHE_H

#define LIGHTS_SHADOW_CACHE_MAX_DEPTH 32

class Ray;
class Object;

class ShadowCache {
    public:
	ShadowCache();
	bool occluded(const Ray& ray_to_light, const double dist_to_light, unsigned int depth, SpaceSubdivider* space);

    private:
	Object* objectlist[LIGHTS_SHADOW_CACHE_MAX_DEPTH];
	Object* get(unsigned int depth) const;
	void put(unsigned int depth, Object* object);
};

#endif

