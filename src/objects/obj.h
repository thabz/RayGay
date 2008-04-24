
#ifndef OBJECTS_OBJ_H
#define OBJECTS_OBJ_H

#include "objects/mesh.h"
#include <string>

/**
 * Loader for Wavefront OBJ meshes.
 * 
 * @see http://www.eg-models.de/formats/Format_Obj.html
 */
class OBJ : public Mesh 
{
    public: 
        OBJ(std::string filename, const Material* material);    
};

#endif
