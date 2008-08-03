
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
    private:
        int readFace(istream& is, int* vertex_idx, int* uv_idx, int* normal_idx, uint32_t max_verts);    
};

#endif
