
#ifndef OBJECTS_PLY_H
#define OBJECTS_PLY_H

#include "objects/mesh.h"

/**
 * A importer for PLY meshes.
 *
 * PLY is the Polygon File Format otherwise known as The Stanford Triangle Format.
 * 
 * @see http://graphics.stanford.edu/data/3Dscanrep/
 * @see http://www.cc.gatech.edu/projects/large_models/
 * 
 */
class Ply : public Mesh {

    public:
	Ply(const std::string& filename, const double scale, const Material* material);
    private:
	std::string readString(std::ifstream& stream);
	int readInt(std::ifstream& stream);
	double readDouble(std::ifstream& stream);

};

#endif

