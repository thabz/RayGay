
#ifndef IMPORTER_H
#define IMPORTER_H

#include <string>
#include <map>

using namespace std;
class Scene;
class Material;
class Path;
class Vector2;
class SceneObject;
class PhotonSettings;

class Importer {

    public:
	Importer(const string& filename);
	Scene* getScene() const { return scene; };
	Vector2 getImageSize() const;
	PhotonSettings* getPhotonSettings() const { return photon_settings; };

    private:
	void parse(const string& filename);
	Path* lookupPath(const string& name);
	Material* lookupMaterial(const string& name);
	Material* initMaterial(const string& name);
	void putNamedObject(const string& key, SceneObject* obj);
	SceneObject* getNamedObject(const string& key);

	map<string,Material*> materials;
	map<string,Path*> paths;
	map<string,SceneObject*> named_objects;
	Scene* scene;
	PhotonSettings* photon_settings;

	double ratio;
	int width;
};

#endif
