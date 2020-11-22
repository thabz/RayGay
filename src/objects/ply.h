
#ifndef OBJECTS_PLY_H
#define OBJECTS_PLY_H

#include "objects/mesh.h"
#include <string>

/**
 * Loader for PLY meshes.
 *
 * @see http://graphics.stanford.edu/data/3Dscanrep/
 * @see http://www-static.cc.gatech.edu/projects/large_models/
 */
class PLY : public Mesh {
public:
  PLY(std::string filename, const Material *material);
};

#endif
