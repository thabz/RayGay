
#include "importer.h"
#include "scene.h"
#include <fstream>
#include <iostream>
#include "camera.h"
#include "box.h"
#include "cylinder.h"
#include "extrusion.h"
#include "sphere.h"
#include "paths/linesegment.h"
#include "paths/circle.h"
#include "lights/pointlight.h"
#include "materials/material.h"

Importer::Importer(const std::string& filename) {
    this->filename = filename;
    scene = new Scene();
    parse();
}

Vector readVector(std::ifstream& stream) {
    double d1,d2,d3;
    stream >> d1;
    stream >> d2;
    stream >> d3;
    return Vector(d1,d2,d3);
}

double readDouble(std::ifstream& stream) {
    double d1;
    stream >> d1;
    return d1;
}

int readInt(std::ifstream& stream) {
    int d1;
    stream >> d1;
    return d1;
}

std::string readString(std::ifstream& stream) {
    string s;
    stream >> s;
    return s;
}

Material* Importer::lookupMaterial(const string& material_name) {
    Material* material = materials[material_name];
    if (material == NULL) {
	cerr << "Unknown material '" << material_name << "'" << endl;
    }
    return material;
}

Path* Importer::lookupPath(const string& path_name) {
    Path* path = paths[path_name];
    if (path == NULL) {
	cerr << "Unknown path '" << path_name << "'" << endl;
    }
    return path;
}

Material* Importer::initMaterial(const string& material_name) {
    Material* new_material = new Material();
    materials[material_name] = new_material;
    return new_material;
}

void Importer::parse() {
    std::ifstream stream(filename.c_str());

    if (stream.bad()) {
    	std::cerr << "Unable to open " << filename << std::endl;
        exit(EXIT_FAILURE);
    }

    Material* cur_material = new Material();
    RGB cur_rgb;

    std::string command;
    std::string str1;
    std::string str2;
    std::string str3;

    Camera* camera = new Camera(Vector(0,0,1500),Vector(0,0,-1));
    scene->setCamera(camera);

    while(!stream.eof()) {
	stream >> command;

	if (!stream) {
	    return;
	}

	if (command == "material") {
	    stream >> str1;
	    cur_material = initMaterial(str1);
	} else if (command == "camera-position") {
	    Vector v = readVector(stream);
	    // TODO
	} else if (command == "camera-lookat") {
	    Vector v = readVector(stream);
	    // TODO
	} else if (command == "camera-aa") {
	    int a = readInt(stream);
	    camera->enableAdaptiveSupersampling(a);
	} else if (command == "background") {
	    RGB col = readVector(stream);
	    scene->setBackgroundColor(col);
	} else if (command == "fog") {
	    double dist = readDouble(stream);
	    RGB col = readVector(stream);
	    scene->setFog(col,dist);
	} else if (command == "diffuse") {
	    cur_material->setDiffuseColor(readVector(stream));
	} else if (command == "specular") {
	    cur_material->setSpecularColor(readVector(stream));
	} else if (command == "specularpower") {
	    cur_material->setSc(readInt(stream));
	} else if (command == "kd") {
	    cur_material->setKd(readDouble(stream));
	} else if (command == "ks") {
	    cur_material->setKs(readDouble(stream));
	} else if (command == "texturemap") {
	    cur_material->setTexturemap(readString(stream));
	} else if (command == "linesegment") {
	    stream >> str1;
	    Vector c1 = readVector(stream);
	    Vector c2 = readVector(stream);
	    Linesegment* l = new Linesegment(c1,c2);
	    paths[str1] = l;
	} else if (command == "pointlight") {
	    Vector c = readVector(stream);
	    Pointlight* l = new Pointlight(c);
	    scene->addLight(l);
	} else if (command == "extrusion") {
	    Material* m = lookupMaterial(readString(stream));
	    Path* p = lookupPath(readString(stream));
	    double r = readDouble(stream);
	    int segments = readInt(stream);
	    int pieces = readInt(stream);
	    Extrusion* e = new Extrusion(*p,r,segments,pieces,*m);
	    scene->addObject(e);
	} else if (command == "sphere") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double r = readDouble(stream);
	    Vector c = readVector(stream);
	    Sphere* s = new Sphere(c,r,*m);
	    scene->addObject(s);
	} else if (command == "cylinder") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double r = readDouble(stream);
	    Vector c1 = readVector(stream);
	    Vector c2 = readVector(stream);
	    Cylinder* cyl = new Cylinder(c1,c2,r,*m);
	    scene->addObject(cyl);
	} else if (command == "box") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    Vector c1 = readVector(stream);
	    Vector c2 = readVector(stream);
	    Box* box = new Box(c1,c2,*m);
	    scene->addObject(box);
	} else {
            cerr << "Unknown " << command << endl;
	    exit(EXIT_FAILURE);


	}
    }
}

