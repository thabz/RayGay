
#include "objects/obj.h"
#include "exception.h"
#include <string>
#include <fstream>

using namespace std;

OBJ::OBJ(string filename, const Material* m) : Mesh(Mesh::MESH_PHONG, m)
{
    uint32_t faces = 0, verts = 0;
    char line[2000];
    ifstream file(filename.c_str(), ios::in);
    uint32_t vertex_line_elements = 0;
    
    if (!file.is_open()) {
        throw_exception("Error opening file. Wrong filename?");     
    }
    
    file >> line;
    if (string(line) != "ply") {
        throw_exception("Not a PLY file");            
    }

    double x,y,z;
    int p1,p2,p3,p4;
    string mode;
    
    while (!file.eof()) {
        file >> mode;
        if (mode[0] == '#') {
            do {} while (file.get() != '\n');
        } else if (mode == "v") {
            file >> x;    
            file >> y;    
            file >> z;
        } else if (mode == "f") {
            
        }
    }

    double x,y,z,ignored;
    while(verts-- > 0) {
        file >> x;    
        file >> y;    
        file >> z;
        for(uint32_t i = 3; i < vertex_line_elements; i++) {
           file >> ignored;        
        }
        addVertex(Vector(x,y,z));
    }

    uint32_t idx[4];
    int num;
    while(faces-- > 0) {
        file >> num;
        file >> idx[0];
        file >> idx[1];
        file >> idx[2];
        if (num == 3) {
            addTriangle(idx);        
        } else if (num == 4) {
           file >> idx[3];
           addQuad(idx);        
        } else {
           throw_exception("Too many verts in face. Only 3 or 4 supported.");        
        }
    }
    file.close();
    
    cout << "Done." << endl;
}


