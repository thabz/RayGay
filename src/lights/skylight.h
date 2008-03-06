#ifndef SKYLIGHT_H 
#define SKYLIGHT_H 

#include "lights/lightsource.h"
#include "lights/shadowcache.h"
#include <vector>

class Object;

/// Hemispherical lightsource
class Skylight: public Lightsource {

    public:
	    Skylight(double radius, int num);
	    virtual ~Skylight() {};
	    void getLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, uint32_t depth) const;
        void transform(const Matrix& m) {};

    private:
	    std::vector<Vector> positions;
	    // TODO: Not threadsafe! Make this thread_local. Identical problem in arealight.
	    mutable std::vector<ShadowCache> shadowcaches;
	    double radius;
	    int num;
};

#endif
