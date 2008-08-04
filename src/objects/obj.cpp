
#include "objects/obj.h"
#include "exception.h"
#include <string>
#include <fstream>

using namespace std;

OBJ::OBJ(string filename, const Material* m) : Mesh(Mesh::MESH_PHONG, m)
{
    ifstream file(filename.c_str(), ios::in);
    
    if (!file.is_open()) {
        throw_exception("Error opening file. Wrong filename?");     
    }
    
    string mode = "  ";
    
    while (!file.eof()) {
        mode = "  ";
        while(file.peek() == ' ' || file.peek() == '\n') { file.get(); }
        int i = 0;
        do {
            mode[i++] = file.get();
        } while (file.peek() != ' ' && file.peek() != '\n');

        if (mode == "#") {
            do {} while (file.get() != '\n');
        } else if (mode == "v") {
            double x,y,z;
            file >> x;    
            file >> y;    
            file >> z;
            addVertex(Vector(x,y,z));
        } else if (mode == "f") {
            int verts[100];
            int uvs[100];
            int normals[100];
            uint32_t num;
            num = readFace(file, verts, uvs, normals, 100);
            if (num == 3) {
                if (uvs[0] == -1) {
                    addTriangle((uint32_t*)verts);
                } else if (normals[0] == -1) {
                    addTriangle((uint32_t*)verts, (uint32_t*)uvs);
                } else {
                    addTriangle((uint32_t*)verts, (uint32_t*)uvs, (uint32_t*)normals);
                }
            } else if (num == 4) {
                if (uvs[0] == -1) {
                    addQuad((uint32_t*)verts);
                } else if (normals[0] == -1) {
                    addQuad((uint32_t*)verts, (uint32_t*)uvs);
                } else {
                    addQuad((uint32_t*)verts, (uint32_t*)uvs, (uint32_t*)normals);
                }
            } else {
               throw_exception("Too many verts in face. Only 3 or 4 supported.");        
            }
        } else if (mode == "vn") {
            double x,y,z;
            file >> x;    
            file >> y;    
            file >> z;
            addNormal(Vector(x,y,z));
        } else if (mode == "vt") {
            double u,v;
            file >> u;
            file >> v;
            addUV(Vector2(u,v));
        } else {
            cerr << mode << " line ignored in " << filename << endl;
        }
    }

    file.close();
    
    cout << "Done." << endl;
}

int OBJ::readFace(istream& is, int* vertex_idx, int* uv_idx, int* normal_idx, uint32_t max_verts) {
    bool done = false;
    uint32_t i = 0;
    int idxs[3];
    while (!done && i < max_verts) {
        idxs[0] = -1; idxs[1] = -1; idxs[2] = -1;
        int j = 0;
        do {
           is >> idxs[j++];
           // The indices in the obj-files are 1-based, so readjust.
           idxs[j++]--; 
        } while (is.peek() == '/' && j < 3);
        while(is.peek() == ' ') { is.get(); }
        if (is.peek() == '\n' || is.eof()) {
            done = true;
        }
        vertex_idx[i] = idxs[0];
        uv_idx[i] = idxs[1];
        normal_idx[i] = idxs[2];
        i++;
    }
    return i;
}
