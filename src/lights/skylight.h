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
        void getSingleLightinfo(const Intersection& inter, KdTree* space, Lightinfo* info, uint32_t depth) const;
        void transform(const Matrix& m) {};

    private:
	    std::vector<Vector> positions;
	    pthread_key_t shadowcaches_key;
	    double radius;
	    int num;

        bool probe(int num, const Ray& ray, double dist, uint32_t depth, KdTree* space) const;

};

#endif
