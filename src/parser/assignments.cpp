
#include "parser/assignments.h"

Assignments* Assignments::getUniqueInstance() {
    static Assignments unique_instance;
    return &unique_instance;
}

Assignments::Assignments() {

}

Path* Assignments::getNamedPath(string name) {
    Path* result = pathMap[name];
    if (result == NULL) {
	//yyerror("path '" + name + "' not defined.");
    } 
    return result;
}

void Assignments::setNamedPath(string name, Path* path) {
    pathMap[name] = path;
}

double Assignments::getNamedFloat(string name) {
    return floatMap[name];
}

void Assignments::setNamedFloat(string name, double val) {
    floatMap[name] = val;
}

Vector Assignments::getNamedVector(string name) {
    return vectorMap[name];
}

void Assignments::setNamedVector(string name, Vector v) {
    vectorMap[name] = v;
}

SceneObject* Assignments::getNamedSceneObject(string name) {
    SceneObject* result = objectMap[name];
    if (result == NULL) {
	//yyerror("scene-object '" + name + "' not defined.");
    }
    return result;
}

void Assignments::setNamedSceneObject(string name, SceneObject* obj) {
    objectMap[name] = obj;
}

Material* Assignments::getNamedMaterial(string name) {
    Material* result = materialMap[name];
    if (result == NULL) {
	//yyerror("material '" + name + "' not defined.");
    }
    return result;
}

void Assignments::setNamedMaterial(string name, Material* material) {
    materialMap[name] = material;
}
