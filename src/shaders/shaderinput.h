
#include "math/vector.h"
#include "math/vector2.h"
#include "intersection.h"
#include "ray.h"

class ShaderInput {
    public:
	Vector* surfaceNormal;
	Intersection* intersection;
	Vector2* uv;
	Ray* ray;
};

