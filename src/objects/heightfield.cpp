
#include "objects/heightfield.h"
#include "image/texture.h"

HeightField::HeightField(Texture* image, double height, double width, double depth, unsigned int width_divisions, unsigned int depth_divisions, const Material* material) : Mesh(Mesh::MESH_FLAT,material) {

	double t_width = 1.0 / double(width_divisions); // Texel width
	double t_depth = 1.0 / double(depth_divisions); // Texel delth
	double d_width = width / double(width_divisions); // Patch width
	double d_depth = depth / double(depth_divisions); // Patch depth

	Vector* c = new Vector[4];
	Vector2* uvs = new Vector2[4];
	int half_width = width_divisions / 2;
	int half_depth = depth_divisions / 2;
	for(int x = -half_width; x < half_width; x++) {
	    for(int z = -half_depth; z < half_depth; z++) {
		double u1 = (x + half_width)*t_width;
		double u2 = (1 + x + half_width)*t_width;
		double v1 = (z + half_depth)*t_depth;
		double v2 = (1 + z + half_depth)*t_depth;
		c[3] = Vector(x*d_width,image->getTexel(u1,v1).brightness() * height,z*d_depth);
		uvs[0] = Vector2(u1,v1);
		c[2] = Vector((x+1)*d_width,image->getTexel(u2,v1).brightness() * height,z*d_depth);
		uvs[1] = Vector2(u2,v1);
		c[1] = Vector((x+1)*d_width,image->getTexel(u2,v2).brightness() * height,(z+1)*d_depth);
		uvs[2] = Vector2(u2,v2);
		c[0] = Vector((x)*d_width,image->getTexel(u1,v2).brightness() * height,(z+1)*d_depth);
		uvs[3] = Vector2(u1,v2);
		addQuad(c,uvs);
	    }
	}
    
}

