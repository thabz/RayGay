
#ifndef OBJECT_3DS_H
#define OBJECT_3DS_H

#include <string>
#include "objectgroup.h"
#include "materials/material.h"
#include "math/matrix.h"

class Vector;
class Vector2;

/**
 * A 3D Studio format object.
 *
 * This objects is groups of Meshes read from a .3ds file.
 *
 * You can find free 3ds-files everywhere, most notably: 
 */
class ThreeDS : public ObjectGroup {

    public:
	ThreeDS(const std::string& filename, const double scale);
        ThreeDS(const std::string& filename, const double scale, const Material* material);

    private:
	void init(const std::string& filename, const double scale);
	void load3ds(const std::string& filename);
	Vector getVertex(const unsigned short index) const;
	Vector2 getUV(const unsigned short index) const;
	void createMesh();

	std::vector<float> vertices;
	std::vector<float> map_coords;
	std::vector<unsigned short> faces;
	std::vector<Material> materials;

	double scale;
	bool force_my_material;
	Matrix mesh_matrix;
	const Material* material;
};

#endif
