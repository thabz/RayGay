
#include <unistd.h>
#include <fstream>
#include <iostream>
#include <cassert>

#include "importer.h"
#include "scene.h"
#include "camera.h"
#include "torus.h"
#include "box.h"
#include "necklace.h"
#include "cylinder.h"
#include "extrusion.h"
#include "math/vector2.h"
#include "sphere.h"
#include "paths/linesegment.h"
#include "paths/circle.h"
#include "paths/spiral.h"
#include "lights/pointlight.h"
#include "lights/arealight.h"
#include "lights/spotlight.h"
#include "materials/material.h"

Importer::Importer(const std::string& filename) {
    ratio = -1;
    scene = new Scene();
    Camera* camera = new Camera();
    scene->setCamera(camera);

    parse(filename);

    // Set image size
    Vector2 s = getImageSize();
    camera->setImageSize(int(s[0]),int(s[1]));
}

Vector2 Importer::getImageSize() const {
    if (ratio != -1) {
	return Vector2(width,double(width)*ratio);
    } else {
	return Vector2(640,480);
    }
}

void Importer::putNamedObject(const string& key, SceneObject* obj) {
    named_objects[key] = obj;
}

SceneObject* Importer::getNamedObject(const string& key) {
    SceneObject* result = named_objects[key];
    if (result == NULL) {
	cerr << "Unknown named object '" << key << "'" << endl;
        exit(EXIT_FAILURE);
    }
    return result;
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
        exit(EXIT_FAILURE);
    }
    return material;
}

Path* Importer::lookupPath(const string& path_name) {
    Path* path = paths[path_name];
    if (path == NULL) {
	cerr << "Unknown path '" << path_name << "'" << endl;
        exit(EXIT_FAILURE);
    }
    return path;
}

Material* Importer::initMaterial(const string& material_name) {
    Material* new_material = new Material();
    materials[material_name] = new_material;
    return new_material;
}

void Importer::parse(const string& filename) {
    std::ifstream stream(filename.c_str());

    if (stream.bad()) {
    	std::cerr << "Unable to open " << filename << std::endl;
        exit(EXIT_FAILURE);
    }

    // Change cwd to this files parent folder
    char original_working_dir[1024];
    getcwd(original_working_dir,1024);
    string original_cwds = string(original_working_dir);
    string cwds = string(original_working_dir) + "/" + filename;
    cout << "Reading " << cwds << endl;
    int idx = cwds.find_last_of('/');
    cwds.resize(idx);
    chdir(cwds.c_str());

    Material* cur_material = new Material();
    RGB cur_rgb;

    std::string command;
    std::string str1;
    std::string str2;
    std::string str3;

    bool naming_object = false;
    string object_name;
    SceneObject* cur_object = NULL;
    SceneObject* last_referenced_object;
    Camera* camera = scene->getCamera();
    int grouping = 0;
    ObjectGroup* cur_group;

    while(!stream.eof()) {
	stream >> command;

	if (!stream) {
	    chdir(original_cwds.c_str());
	    return;
	}

	if (command == "material") {
	    stream >> str1;
	    cur_material = initMaterial(str1);
	} else if (command == "image-width") {
	    this->width = readInt(stream);
	} else if (command == "image-ratio") {
	    double w = readDouble(stream);
	    double h = readDouble(stream);
	    this->ratio = h / w;
	} else if (command == "camera-position") {
	    Vector v = readVector(stream);
	    camera->setPosition(v);
	} else if (command == "camera-up") {
	    Vector v = readVector(stream);
	    camera->setUp(v);
	} else if (command == "camera-field-of-view") {
	    double d = readDouble(stream);
	    camera->setFieldOfView(d);
	} else if (command == "camera-lookat") {
	    Vector v = readVector(stream);
	    camera->setLookAt(v);
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
	} else if (command == "specpow") {
	    cur_material->setSc(readInt(stream));
	} else if (command == "kd") {
	    cur_material->setKd(readDouble(stream));
	} else if (command == "ks") {
	    cur_material->setKs(readDouble(stream));
	} else if (command == "texturemap") {
	    cur_material->setTexturemap(readString(stream));
	} else if (command == "name") {
	    object_name = readString(stream);
	    naming_object = true;
	    cur_object = NULL;
	} else if (command == "group") {
	    grouping = 1;
	    cur_group = new ObjectGroup();
	} else if (command == "end") {
	    if (grouping > 0) {
		grouping = 0;
		cur_object = cur_group;
	    } else {
		cout << "Why end here?" << endl;
		exit(EXIT_FAILURE);
	    }
	} else if (command == "object") {
	    object_name = readString(stream);
	    cur_object = getNamedObject(object_name)->clone();
	} else if (command == "circle") {
	    stream >> str1;
	    Vector center = readVector(stream);
	    double radius = readDouble(stream);
	    Vector normal = readVector(stream);
	    Circle* circle = new Circle(center,radius,normal);
	    paths[str1] = circle;
	} else if (command == "linesegment") {
	    stream >> str1;
	    Vector c1 = readVector(stream);
	    Vector c2 = readVector(stream);
	    Linesegment* l = new Linesegment(c1,c2);
	    paths[str1] = l;
	} else if (command == "spiral") {
	    stream >> str1;
	    stream >> str2;
	    Path* p = lookupPath(str2);
	    double radius = readDouble(stream);
	    double windings = readDouble(stream);
	    double offset = readDouble(stream);
	    Spiral* spiral = new Spiral(p,radius,windings,offset);
	    paths[str1] = spiral;
	} else if (command == "light") {
	    string type = readString(stream);
	    Lightsource* l;
	    if (type == "point") {
		Vector c = readVector(stream);
		l = new Pointlight(c);
	    } else if (type == "area") {
		Vector pos = readVector(stream);
		Vector dir = readVector(stream);
		double r = readDouble(stream);
		int num = readInt(stream);
		double jitter = readDouble(stream);
		l = new Arealight(pos,dir,r,num,jitter);
	    } else if (type == "spot") {
		Vector pos = readVector(stream);
		Vector look_at = readVector(stream);
		double angle = readDouble(stream);
		double cut_angle = readDouble(stream);
		l = new Spotlight(pos,look_at,angle,cut_angle);
	    } else {
		cout << "Unknown type of light" << endl;
		exit(EXIT_FAILURE);
	    }
	    scene->addLight(l);
	} else if (command == "extrusion") {
	    Material* m = lookupMaterial(readString(stream));
	    Path* p = lookupPath(readString(stream));
	    double r = readDouble(stream);
	    int segments = readInt(stream);
	    int pieces = readInt(stream);
	    cur_object = new Extrusion(*p,r,segments,pieces,*m);
	} else if (command == "sphere") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double r = readDouble(stream);
	    Vector c = readVector(stream);
	    cur_object = new Sphere(c,r,*m);
	} else if (command == "torus") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double R = readDouble(stream);
	    double r = readDouble(stream);
	    cur_object = new Torus(R,r,*m);
	} else if (command == "cylinder") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double r = readDouble(stream);
	    Vector c1 = readVector(stream);
	    Vector c2 = readVector(stream);
	    cur_object = new Cylinder(c1,c2,r,*m);
	} else if (command == "box") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    Vector c1 = readVector(stream);
	    Vector c2 = readVector(stream);
	    cur_object = new Box(c1,c2,*m);
	} else if (command == "necklace") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    stream >> str1;
	    Path* p = lookupPath(str1);
	    int num = readInt(stream);
	    double r = readDouble(stream);
	    cur_object = new Necklace(*p,num,r,*m);
	} else if (command == "rotate") {
	    SceneObject* obj = last_referenced_object;
	    assert(obj != NULL);
	    Vector axis = readVector(stream);
	    double angle = readDouble(stream);
	    Matrix m = Matrix::matrixRotate(axis,angle);
	    obj->transform(m);
	} else if (command == "translate") {
	    SceneObject* obj = last_referenced_object;
	    assert(obj != NULL);
	    Vector axis = readVector(stream);
	    Matrix m = Matrix::matrixTranslate(axis);
	    obj->transform(m);
	} else if (command == "include") {
	    string filename = readString(stream);
	    parse(filename);
	} else if (command[0] == '#') {
	    // Comment. Ignore rest of line.
	    while (stream.get() != '\n') {
	    }
	} else {
            cerr << "Unknown " << command << endl;
	    exit(EXIT_FAILURE);
	}

	// TODO: Last object in a group is currently ignored...
	if (cur_object != NULL) {
	    if (naming_object && grouping == 0) {
		putNamedObject(object_name,cur_object);
		naming_object = false;
	    }
	    if (grouping == 1) {
		cur_object = cur_group;
		grouping = 2;
	    } else if (grouping == 2) {
		cur_group->addObject(cur_object);
	    } else {
		scene->addObject(cur_object);
	    }
	    last_referenced_object = cur_object;
	    cur_object = NULL;
	}
    }
    chdir(original_cwds.c_str());
}

