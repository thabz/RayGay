
#ifndef PARSER_ASSIGNMENTS_H
#define PARSER_ASSIGNMENTS_H

#include <string>
#include <map>

class Path;
class SceneObject;
class Material;

using namespace std;

class Assignments {

    public:
	static Assignments* Assignments::getUniqueInstance();
	
	Path* getNamedPath(string name);
	void setNamedPath(string name, Path* path_node);

	double getNamedFloat(string name);
	void setNamedFloat(string name, double val);
	
	Material* getNamedMaterial(string name);
	void setNamedMaterial(string name, Material* material);

	SceneObject* getNamedSceneObject(string name);
	void setNamedSceneObject(string name, SceneObject* obj_node);

    private:
	Assignments(); 

	map<string,double> floatMap;
	map<string,Path*> pathMap;
	map<string,Material*> materialMap;
	map<string,SceneObject*> objectMap;
};

#endif
