#include <fstream>
#include <iostream>
#include <cassert>
#include <string>

#include "objects/ply.h"
#include "exception.h"

using namespace std;

std::string Ply::readString(std::ifstream& stream) {
    string s;
    stream >> s;
    return s;
}

double Ply::readDouble(std::ifstream& stream) {
    double d1;
    stream >> d1;
    return d1;
}

int Ply::readInt(std::ifstream& stream) {
    int d1;
    stream >> d1;
    return d1;
}

Ply::Ply(const std::string& filename, const double scale, const Material* m) : Mesh(Mesh::MESH_FLAT,m) {
    std::ifstream stream(filename.c_str());

    if (stream.fail()) {
	throw_exception("Unable to open " + filename);
    }
    
    int face_num = -1;
    int vertex_num = -1;

    // Read header stuff
    string token;
    do {
	if (token == "element") {
	    token = readString(stream);
	    if (token == "vertex") {
		vertex_num = readInt(stream);
	    } else if (token == "face") {
		face_num = readInt(stream);
	    }
	}
	token = readString(stream);	
    } while (token != "end_header");

    assert(face_num != -1);
    assert(vertex_num != -1);

    // Read vertex vectors
    vector<Vector> vertices;
    for(int i = 0; i < vertex_num; i++) {
	double d1,d2,d3;
	stream >> d1;
	stream >> d2;
	stream >> d3;
	vertices.push_back(Vector(d1,d2,d3));
    }
    cout << "Read vertices..." << endl;

    // Read and create faces
    for(int i = 0; i < face_num; i++) {
	int index[4];
	int num = readInt(stream);
	if (num == 3) {
	    index[0] = readInt(stream);
	    index[1] = readInt(stream);
	    index[2] = readInt(stream);
	    addTriangle(vertices[index[0]],vertices[index[1]],vertices[index[2]]);
	} else {
	    cout << "Face with 4 corners!";
	}
    }
}


