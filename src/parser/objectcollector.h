
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
	ObjectListNode* popAsListNode();
	/// Adds an object to the toplevel collection
	void addObject(SceneObjectNode* node);

    private:
	list< list< SceneObjectNode*>* > the_stack;
};

#endif


