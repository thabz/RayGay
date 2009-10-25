#ifndef TRIANGLE_H
#define TRIANGLE_H

#include <pthread.h>
#include "object.h"

class Material;
class BoundingBox;
class Mesh;
class Intersection;
class Matrix;
class Triangle;

#define CACHE_ENTRIES 512 

// If backface culling is enabled, away-pointing triangle
// are invisible. Thus no intersection registers when shooting
// a ray from inside a mesh. Backface culling gives faster
// triangle-intersection checking. Also it gives more robust
// intersection checking for shadow rays originating on the
// surface.
#define TRIANGLE_BACKFACE_CULLING

struct CachedVertex {
    double vert0[3], vert1[3], vert2[3];
    double edge1[3], edge2[3];
    const Triangle* triangle;
    double last_t;
    int64_t last_ray_id;
}; // 144 bytes


class TriangleVertexCache {
    public:
	TriangleVertexCache();
	CachedVertex* getCachedVertex(const Triangle* triangle) const;
    private:
	pthread_key_t pthread_key;
};

/// The triangle of a Mesh
class Triangle : public Object {
    friend class TriangleVertexCache;

    public:
	/// Constructor
	Triangle(Mesh* m, uint32_t tri_index);

	void transform(const Matrix& m) { };
	const Material* getMaterial() const;
	AABox getBoundingBox() const;

	void prepare();

	virtual SceneObject* clone() const { return NULL; };
	double _fastIntersect(const Ray& ray) const;
	void _fullIntersect(const Ray& ray, const double t, Intersection& result) const;
	double area() const;
        int intersects(const AABox& voxel_bbox, const AABox& obj_bbox) const;
	bool canSelfshadow() const;

    private:
	Mesh* mesh;
	uint32_t _tri_idx;
};

#endif
