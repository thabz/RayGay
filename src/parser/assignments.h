
#ifndef PARSER_ASSIGNMENTS_H
#define PARSER_ASSIGNMENTS_H

#include <string>
#include <map>

class FloatNode;
class PathNode;
class SceneObjectNode;
class MaterialNode;

using namespace std;

class Assignments {

    public:
	static Assignments* getUniqueInstance();
	
	PathNode* getNamedPath(string name);
	void setNamedPath(string name, PathNode* path_node);

	FloatNode* getNamedFloat(string name);
	void setNamedFloat(string name, FloatNode* float_node);
	
	MaterialNode* getNamedMaterial(string name);
	void setNamedMaterial(string name, MaterialNode* material_node);

	SceneObjectNode* getNamedSceneObject(string name);
	void setNamedSceneObject(string name, SceneObjectNode* obj_node);

    private:
	Assignments();
	static Assignments* Assignments::unique_instance;

	map<string,FloatNode*> floatMap;
	map<string,PathNode*> pathMap;
	map<string,MaterialNode*> materialMap;
	map<string,SceneObjectNode*> objectMap;
};

#endif
