
#include <unistd.h>
#include <fstream>
#include <iostream>
#include <cassert>
#include <vector>

#include "importer.h"
#include "scene.h"
#include "camera.h"
#include "objects/torus.h"
#include "objects/3ds.h"
#include "objects/blob.h"
#include "objects/box.h"
#include "objects/sor.h"
#include "objects/necklace.h"
#include "objects/cylinder.h"
#include "objects/extrusion.h"
#include "objects/heightfield.h"
#include "objects/sphere.h"
#include "objects/transformedinstance.h"
#include "paths/linesegment.h"
#include "paths/circle.h"
#include "paths/spiral.h"
#include "renderersettings.h"
#include "lights/pointlight.h"
#include "lights/arealight.h"
#include "lights/spotlight.h"
#include "lights/skylight.h"
#include "materials/materials.h"
#include "image/rgba.h"
#include "image/image.h"
#include "math/vector2.h"

Importer::Importer(const std::string& filename) {
    ratio = -1;
    scene = new Scene();
    Camera* camera = new Camera();
    scene->setCamera(camera);

    renderer_settings = new RendererSettings();

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

RGBA readRGBA(std::ifstream& stream) {
    double r,g,b,a;
    stream >> r;
    stream >> g;
    stream >> b;
    stream >> a;
    return RGBA(r,g,b,a);
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

void Importer::registerMaterial(const string& name, Material* material) {
    materials[name] = material;
}

void Importer::parse(const string& filename) {
    std::ifstream stream(filename.c_str());

    if (stream.fail()) {
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
    SceneObject* last_referenced_object = NULL;
    Camera* camera = scene->getCamera();
    int grouping = 0;
    ObjectGroup* cur_group = NULL;

    while(!stream.eof()) {
	stream >> command;

	if (!stream) {
	    chdir(original_cwds.c_str());
	    return;
	}

	if (command == "material") {
	    stream >> str1;
	    string mat_type = readString(stream);
	    if (mat_type == "plastic") {
		RGB col = readVector(stream);
		cur_material = new Plastic(col);
	    } else if (mat_type == "wood") {
		RGB col1 = readVector(stream);
		RGB col2 = readVector(stream);
		cur_material = new Wood(col1,col2);
	    } else if (mat_type == "marble") {
		RGB col1 = readVector(stream);
		RGB col2 = readVector(stream);
		cur_material = new Marble(col1,col2);
	    } else if (mat_type == "checker") {
		Material* mat1 = lookupMaterial(readString(stream));
		Material* mat2 = lookupMaterial(readString(stream));
		double size = readDouble(stream);
		cur_material = new Checker(mat1,mat2,size);
	    } else {
		cout << "Unknown materialtype: " << mat_type << endl;
		exit(EXIT_FAILURE);
	    }
	    registerMaterial(str1,cur_material);
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
	    RGBA col = readRGBA(stream);
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
	} else if (command == "noshadow") {
	    cur_material->setNoShadow(true);
	} else if (command == "ks") {
	    cur_material->setKs(readDouble(stream));
	} else if (command == "kt") {
	    cur_material->setKt(readDouble(stream));
	} else if (command == "eta") {
	    cur_material->setEta(readDouble(stream));
	} else if (command == "gloss") {
	    int rays = readInt(stream);
	    double angle = readDouble(stream);
	    cur_material->enableGloss(rays,angle);
	} else if (command == "texturemap") {
	    cur_material->setTexturemap(readString(stream));
	} else if (command == "bumpmap") {
	    string filename = readString(stream);
	    double height = readDouble(stream);
	    cur_material->setBumpmap(filename,height);
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
		assert(cur_group != NULL);
		cur_object = cur_group;
	    } else {
		cout << "Why end here?" << endl;
		exit(EXIT_FAILURE);
	    }
	} else if (command == "object") {
	    object_name = readString(stream);
	    cur_object = getNamedObject(object_name)->clone();
	} else if (command == "transinstance") {
	    object_name = readString(stream);
	    SceneObject* sobj = getNamedObject(object_name);
	    Object* obj = dynamic_cast<Object*>(sobj);
	    if (obj == NULL) {
		cout << "Error creating transformed instance: " << object_name << " is not an Object." << endl;
		exit(EXIT_FAILURE);
	    }
	    cur_object = new TransformedInstance(obj);
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
	} else if (command == "photonmap") {
	    string name = readString(stream);
	    if (name == "globalphotons") {
		renderer_settings->global_photons_num = readInt(stream);
	    } else if (name == "causticphotons") {
		renderer_settings->caustic_photons_num = readInt(stream);
	    } else if (name == "estimate-radius") {
		renderer_settings->estimate_radius = readInt(stream);
	    } else if (name == "estimate-samples") {
		renderer_settings->estimate_samples = readInt(stream);
	    } else if (name == "final-gather-rays") {
		renderer_settings->final_gather_rays = readInt(stream);
	    } else if (name == "cache-tolerance") {
		renderer_settings->cache_tolerance = readDouble(stream);
	    }
	} else if (command == "renderer") {
	    string name = readString(stream);
	    if (name == "raytracer") {
		renderer_settings->renderertype = RendererSettings::RAYTRACER;
	    } else if (name == "photonrenderer") {
		renderer_settings->renderertype = RendererSettings::PHOTON_RENDERER;
	    } else {
		cout << "Unknown renderer" << endl;
		exit(EXIT_FAILURE);
	    }
	} else if (command == "light") {
	    string type = readString(stream);
	    Lightsource* l;
	    if (type == "point") {
		Vector c = readVector(stream);
		Vector power = readVector(stream);
		l = new Pointlight(c);
		l->setPower(power);
	    } else if (type == "skylight") {
		double r = readDouble(stream);
		int num = readInt(stream);
		Vector power = readVector(stream);
		l = new Skylight(r,num);
		l->setPower(power);
	    } else if (type == "area") {
		Vector pos = readVector(stream);
		Vector dir = readVector(stream);
		Vector power = readVector(stream);
		double r = readDouble(stream);
		int num = readInt(stream);
		double jitter = readDouble(stream);
		l = new Arealight(pos,dir,r,num,jitter);
		l->setPower(power);
	    } else if (type == "spot") {
		Vector pos = readVector(stream);
		Vector look_at = readVector(stream);
		Vector power = readVector(stream);
		double angle = readDouble(stream);
		double cut_angle = readDouble(stream);
		l = new Spotlight(pos,look_at,angle,cut_angle);
		l->setPower(power);
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
	    cur_object = new Extrusion(*p,r,segments,pieces,m);
	} else if (command == "sor") {
	    Material* m = lookupMaterial(readString(stream));
	    int segments = readInt(stream);
	    int points_num = readInt(stream);
	    vector<Vector2> points = vector<Vector2>(points_num);
	    
	    for(int i = 0; i < points_num; i++) {
		Vector2 v = Vector2(readDouble(stream),readDouble(stream));
		points.push_back(v);
	    }
	    cur_object = new SurfaceOfRevolution(points,segments,m);
	} else if (command == "sphere") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double r = readDouble(stream);
	    Vector c = readVector(stream);
	    cur_object = new Sphere(c,r,m);
	} else if (command == "blob") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double iso = readDouble(stream);
	    unsigned int steps = readInt(stream);
	    double accuracy = readDouble(stream);
	    Blob* blob = new Blob(iso,steps,accuracy,m);
	    int balls_num = readInt(stream);
	    for(int i = 0; i < balls_num; i++) {
		double weight = readDouble(stream);
		double radius = readDouble(stream);
		Vector center = readVector(stream);
		blob->addAtom(center,radius,weight);
	    }
	    cur_object = blob;
	} else if (command == "torus") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double R = readDouble(stream);
	    double r = readDouble(stream);
	    cur_object = new Torus(R,r,m);
	} else if (command == "heightfield") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    string filename = readString(stream);
	    Image* img = Image::load(filename);
	    double height = readDouble(stream);
	    double width = readDouble(stream);
	    double depth = readDouble(stream);
	    int width_divisions = readInt(stream);
	    int depth_divisions = readInt(stream);
	    cur_object = new HeightField(img,height,width,depth,width_divisions,depth_divisions,m);
	    delete img;
	} else if (command == "3ds") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double scale = readDouble(stream);
	    string filename = readString(stream);
	    cur_object = new ThreeDS(filename,scale,m);
	} else if (command == "cylinder") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    double r = readDouble(stream);
	    Vector c1 = readVector(stream);
	    Vector c2 = readVector(stream);
	    cur_object = new Cylinder(c1,c2,r,m);
	} else if (command == "box") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    Vector c1 = readVector(stream);
	    Vector c2 = readVector(stream);
	    cur_object = new Box(c1,c2,m);
	} else if (command == "necklace") {
	    stream >> str1;
	    Material* m = lookupMaterial(str1);
	    stream >> str1;
	    Path* p = lookupPath(str1);
	    int num = readInt(stream);
	    double r = readDouble(stream);
	    cur_object = new Necklace(*p,num,r,m);
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
	    } else if (grouping == 1) {
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

