
#include "objects/heightfield.h"
#include "image/texture.h"
#include <cassert>

HeightField::HeightField(Texture* image, double height, double width, double depth, unsigned int width_divisions, unsigned int depth_divisions, const Material* material) : Mesh(Mesh::MESH_PHONG,material) {

	double t_width = 1.0 / double(width_divisions); // Texel width
	double t_depth = 1.0 / double(depth_divisions); // Texel delth
	double d_width = width / double(width_divisions); // Patch width
	double d_depth = depth / double(depth_divisions); // Patch depth

	uint c[4];
	Vector2 uvs[4];
	int half_width = width_divisions / 2;
	int half_depth = depth_divisions / 2;

	hintVertexNum((depth_divisions+1) * (width_divisions+1));
	uint num = 0;
	Vector vec;
	for(int x = -half_width; x <= half_width; x++) {
	    for(int z = -half_depth; z <= half_depth; z++) {
		double u = (x + half_width)*t_width;
		double v = (z + half_depth)*t_depth;
		double tex_h = image->getTexel(u,v).brightness();
		vec = Vector(x*d_width, tex_h * height,z*d_depth);
		addVertex(vec);
		num++;
	    }
	}
	assert(num == (depth_divisions+1) * (width_divisions+1));

	/*   c0         c1
	 * x0 z0 ---- x1 z0
	 *   |          |
	 *   |          |
	 * x0 z1 ---- x1 z1
	 *   c3         c2
	 */ 
	num = 0;
	hintFaceNum(2 * depth_divisions * width_divisions);
	for(uint x = 0; x < width_divisions; x++) {
	    for(uint z = 0; z < depth_divisions; z++) {
		double u1 = (0 + x)*t_width;
		double u2 = (1 + x)*t_width;
		double v1 = (0 + z)*t_depth;
		double v2 = (1 + z)*t_depth;
		
		uvs[0] = Vector2(u1,v1);
		uvs[1] = Vector2(u2,v1);
		uvs[2] = Vector2(u2,v2);
		uvs[3] = Vector2(u1,v2);

		c[0] = (x+0) + (z+0) * (width_divisions+1);
		c[1] = (x+1) + (z+0) * (width_divisions+1);
		c[2] = (x+1) + (z+1) * (width_divisions+1);
		c[3] = (x+0) + (z+1) * (width_divisions+1);

		addQuad(c,uvs);
		num++;
	    }
	}
	assert(num == depth_divisions * width_divisions);
}

