
#ifndef IMPORTER_H
#define IMPORTER_H

#include <string>
#include <map>

using namespace std;
class Scene;
class Material;
class Path;

class Importer {

    public:
	Importer(const string& filename);
	Scene* getScene() const { return scene; };

    private:
	void parse();
	Path* lookupPath(const string& name);
	Material* lookupMaterial(const string& name);
	Material* initMaterial(const string& name);

	string filename;
	map<string,Material*> materials;
	map<string,Path*> paths;
	Scene* scene;
};

#endif
