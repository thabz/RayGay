
#ifndef OBJECTCOLLECTOR
#define OBJECTCOLLECTOR

#include <list>
#include <vector>
#include "sceneobjectnodes.h"

using namespace std;

class ObjectCollector {

    public:
	/// Constructor
	ObjectCollector();
	/// Reset
	void reset();
	/// Begin a new collection
	void pushCollection();
	/// Pops the toplevels collection
	vector<SceneObject*> pop();
	/// Adds an object to the toplevel collection
	void addObject(SceneObject* node);

    private:
	list< list< SceneObject*>* > the_stack;
};

#endif


