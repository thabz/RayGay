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

#define CACHE_ENTRIES 1024

struct CachedVertex {
    double vert0[3], vert1[3], vert2[3];
    double edge1[3], edge2[3];
    const Triangle* triangle;
    long last_ray_id;
    double last_t;
};


class TriangleVertexCache {
    public:
	TriangleVertexCache();
	const CachedVertex* getCachedVertex(const Triangle* triangle) const;
	
	mutable CachedVertex cached_vertices[CACHE_ENTRIES];
	mutable int next_free_slot;
	mutable pthread_mutex_t mutex;
};

/// The triangle of a Mesh
class Triangle : public Object {
    friend class TriangleVertexCache;

    public:
	/// Constructor
	Triangle(Mesh* m, uint tri_index);

	void transform(const Matrix& m) { };
	const Material* getMaterial() const;
	BoundingBox boundingBoundingBox() const;

	void prepare();

	virtual SceneObject* clone() const { return NULL; };
	double _fastIntersect(const Ray& ray) const;
	Intersection _fullIntersect(const Ray& ray, const double t) const;
	double area() const;
        int intersects(const BoundingBox& voxel_bbox, const BoundingBox& obj_bbox) const;
	//static TriangleVertexCache vertex_cache;

    private:
	Mesh* mesh;
	unsigned int _tri_idx;
	mutable unsigned int last_cache_key;
};

#endif
