
#include "math/vector.h"
#include "math/vector2.h"
#include "intersection.h"
#include "ray.h"

class ShaderInput {
    public:
	/// Intersection point
	Vector& P;
	/// Surface normal at intersection point
	Vector& N;
	/// Inci:ray direction
	Vector& I;
	/// Ray origin
	Vector& E;
	/// Surface texture coordinates
	Vector2& uv;
};
