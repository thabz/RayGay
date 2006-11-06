
#include "objects/ply.h"
#include "exception.h"
#include <string>
#include <fstream>

#define PLY_DEBUG 1

// PLY header format below
/*
ply
format
ascii
1.0
comment
generated
by
ply_writer
element
vertex
5205
property
float
x
property
float
y
property
float
z
element
face
11102
property
list
uchar
int
vertex_indices
end_header
*/



using namespace std;

PLY::PLY(string filename, const Material* m) : Mesh(Mesh::MESH_PHONG, m)
{
    uint32_t faces, verts;        
    char line[2000];
    bool done = false;
    fstream file(filename.c_str(), ios::in);
    
    file >> line;
    if (string(line) != "ply") {
        throw_exception("Not a PLY file");            
    }

    while(!done) {
        file >> line;    
        if (string(line) == string("element")) {
           file >> line;    
            if (string(line) == string("vertex")) {
                file >> verts;
                cout << "Verts: " << verts << endl;    
            } else if (string(line) == string("face")) {
                file >> faces;
                cout << "Faces: " << faces << endl;    
            }
        }
        if (string(line) == "end_header") {
           done = true;        
        }
    }

    double x,y,z;
    while(verts-- > 0) {
        file >> x;    
        file >> y;    
        file >> z;
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
}
