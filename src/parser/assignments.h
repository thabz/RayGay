
#ifndef PARSER_ASSIGNMENTS_H
#define PARSER_ASSIGNMENTS_H

#include <string>
#include <map>
#include "math/vector.h"
#include "fileposition.h"

class Path;
class SceneObject;
class Material;
class LangFunction;

using namespace std;

class Assignments {

    public:
	static Assignments* Assignments::getUniqueInstance();
	
	Path* getNamedPath(string name, FilePosition pos);
	void setNamedPath(string name, Path* path_node);

	double getNamedFloat(string name, FilePosition pos);
	void setNamedFloat(string name, double val);

	Vector getNamedVector(string name, FilePosition pos);
	void setNamedVector(string name, Vector val);
	
	Material* getNamedMaterial(string name, FilePosition pos);
	void setNamedMaterial(string name, Material* material);

	SceneObject* getNamedSceneObject(string name, FilePosition pos);
	void setNamedSceneObject(string name, SceneObject* obj_node);

	LangFunction* getNamedFunction(string name, FilePosition pos);
	void setNamedFunction(string name, LangFunction* function);

    private:
	Assignments(); 

	map<string,double> floatMap;
	map<string,Vector> vectorMap;
	map<string,Path*> pathMap;
	map<string,Material*> materialMap;
	map<string,SceneObject*> objectMap;
	map<string,LangFunction*> functionMap;
};

#endif
