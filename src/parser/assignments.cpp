
#include "parser/assignments.h"
#include "parser/floatnodes.h"
#include "parser/materialnodes.h"
#include "parser/pathnodes.h"
#include "parser/sceneobjectnodes.h"

Assignments* Assignments::unique_instance = NULL;

Assignments* Assignments::getUniqueInstance() {
    if (unique_instance == NULL) {
	unique_instance = new Assignments();
    }
    return unique_instance;
}

PathNode* Assignments::getNamedPath(string name) {
    PathNode* result = pathMap[name];
    if (result == NULL) {
	//yyerror("path '" + name + "' not defined.");
    } 
    return result;
}

void Assignments::setNamedPath(string name, PathNode* path) {
    pathMap[name] = path;
}

FloatNode* Assignments::getNamedFloat(string name) {
    FloatNode* result = floatMap[name];
    if (result == NULL) {
	//yyerror("float '" + name + "' not defined.");
    } 
    return result;
}

void Assignments::setNamedFloat(string name, FloatNode* val) {
    floatMap[name] = val;
}

SceneObjectNode* Assignments::getNamedSceneObject(string name) {
    SceneObjectNode* result = objectMap[name];
    if (result == NULL) {
	//yyerror("scene-object '" + name + "' not defined.");
    }
    return result;
}

void Assignments::setNamedSceneObject(string name, SceneObjectNode* obj) {
    objectMap[name] = obj;
}

MaterialNode* Assignments::getNamedMaterial(string name) {
    MaterialNode* result = materialMap[name];
    if (result == NULL) {
	//yyerror("material '" + name + "' not defined.");
    }
    return result;
}

void Assignments::setNamedMaterial(string name, MaterialNode* material) {
    materialMap[name] = material;
}
